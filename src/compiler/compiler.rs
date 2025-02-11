use std::{collections::HashMap, fmt::Error, path::PathBuf};

use inkwell::{attributes::{Attribute, AttributeLoc}, basic_block::BasicBlock, builder::Builder, context::Context, module::{Linkage, Module}, passes::PassManager, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine}, types::{BasicType, BasicTypeEnum, FunctionType, StructType}, values::{BasicMetadataValueEnum, BasicValue, FunctionValue, PointerValue}, AddressSpace, IntPredicate, OptimizationLevel};

use crate::{ast::{ast::TypeType, types::Literals}, type_checker::{type_checker::TypeChecker, typed_ast::{TypedBlockStmt, TypedStmt, TypedStmtWrapper}}};

use super::stmt::gen_statement;

pub struct Compiler<'a> {
    pub is_stdlib: bool,

    pub ast: TypedBlockStmt,
    pub type_checker: TypeChecker,
    pub current_environment: i32,

    pub named_allocas: HashMap<String, PointerValue<'a>>,
    pub named_structs: HashMap<String, StructType<'a>>,

    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>
}

impl<'a> Compiler<'a> {
    pub fn new(is_stdlib: bool, ast: TypedBlockStmt, type_checker: TypeChecker, context: &'a Context, file_name: &str) -> Self {
        Compiler {
            is_stdlib,
            ast,
            type_checker,
            current_environment: 0,
            module: context.create_module(file_name),
            builder: context.create_builder(),
            context,
            named_allocas: HashMap::new(),
            named_structs: HashMap::new()
        }
    }

    pub fn save_module_to_file(&self, output_file: PathBuf) {
        self.module.print_to_file(output_file).unwrap();
        // println!("{}", self.module.print_to_string().to_str().unwrap());
    }

    fn run_passes(&self) {
        let fpm = PassManager::create(());

        // fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_merge_functions_pass();
        fpm.add_aggressive_dce_pass();
        fpm.add_instruction_simplify_pass();
        fpm.add_dead_arg_elimination_pass();
        fpm.add_dead_store_elimination_pass();
        fpm.add_partially_inline_lib_calls_pass();
        fpm.add_loop_unroll_pass();
        fpm.add_loop_unswitch_pass();
        fpm.add_loop_vectorize_pass();
        fpm.add_loop_deletion_pass();
        fpm.add_memcpy_optimize_pass();
        fpm.add_constant_merge_pass();
        fpm.add_strip_symbol_pass();
        fpm.add_verifier_pass();

        fpm.run_on(&self.module);
    }

    fn compile(&mut self) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();
    
        self.module.set_triple(&target_triple);
        self.module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        self.create_external_functions();
        self.declare_stdlib_functions();

        self.create_function("main", self.context.i32_type().fn_type(&[], false));

        self.gen();

        self.builder.build_return(Some(&self.context.i32_type().const_zero())).unwrap();

        self.run_passes();
    }

    pub fn gen(&mut self) {
        // let string = self.builder.build_global_string_ptr("Hello, world!\n", "").unwrap().as_pointer_value();
        // let printf_fn = self.module.get_function("printf").unwrap();

        // let args = &[string.into()];

        // self.builder.build_call(printf_fn, args, "").unwrap();

        self.current_environment = self.ast.id;
        self.type_checker.clear_environment_path();
        self.type_checker.set_current_environment(self.current_environment);

        let statements = self.ast.body.iter().map(|stmt| stmt.clone_typed_wrapper()).collect::<Vec<TypedStmtWrapper>>();
        for statement in statements.iter() {
            gen_statement(self, statement);
        }
    }

    pub fn convert_type(&self, type_: TypeType) -> BasicTypeEnum<'a> {
        match type_ {
            TypeType::Symbol(_) => {
                panic!("Attempted to convert a symbol type to a basic type");
            },
            TypeType::Array(_) => self.context.i32_type().ptr_type(AddressSpace::default()).into(),
            TypeType::Literal(Literals::String) => self.context.i8_type().ptr_type(AddressSpace::default()).into(),
            TypeType::Literal(Literals::Number) => self.context.i32_type().into(),
            TypeType::Literal(Literals::Boolean) => self.context.bool_type().into(),
            TypeType::Literal(Literals::Null) => self.context.i8_type().ptr_type(AddressSpace::default()).into(),
            TypeType::Literal(Literals::InternalI8Pointer) => {
                if !self.is_stdlib {
                    panic!("Use of internal type outside of stdlib");
                }
                self.context.i8_type().ptr_type(AddressSpace::default()).into()
            },
            TypeType::Struct(name) => {
                let struct_type = self.named_structs.get(&name).unwrap();
                struct_type.ptr_type(AddressSpace::default()).into()
            },
            _ => {
                println!("{:?}", type_);
                todo!()
            }
        }
    }

    pub fn declare_stdlib_functions(&self) {
        let assert_std_works_type = self.context.void_type().fn_type(&[self.context.i8_type().ptr_type(AddressSpace::default()).into()], false);
        self.module.add_function("assert_std_works", assert_std_works_type, Some(Linkage::External));
    }

    pub fn create_external_functions(&self) {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();

        let syscall_type = self.context.i64_type().fn_type(&[i64_type.into();4], false);
        self.module.add_function("syscall", syscall_type, Some(Linkage::External));

        let abort_type = self.context.void_type().fn_type(&[], false);
        self.module.add_function("abort", abort_type, Some(Linkage::External));

        self.create_strlen_function();
        self.create_clone_string_function();

        // int printf(const char* format, ...)
        let printf_type = self.context.i32_type().fn_type(&[i8_ptr_type.into()], true);
        self.module.add_function("printf", printf_type, Some(Linkage::External));

        // int print(const char* format, ...) (this is just a wrapper for printf)
        let print_type = self.context.void_type().fn_type(&[i8_ptr_type.into()], true);
        self.create_print_function(print_type);

        self.create_panic_handler();

        // int scanf(const char* format, ...)
        let scanf_type = self.context.i32_type().fn_type(&[i8_ptr_type.into()], true);
        self.module.add_function("scanf", scanf_type, Some(Linkage::External));

        let input_int_type = self.context.i32_type().fn_type(&[], false);
        self.create_input_int_function(input_int_type);

        // i8* strcat(i8* dest, i8* src)
        let strcat_type = self.context.i8_type().ptr_type(AddressSpace::default()).fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        self.module.add_function("strcat", strcat_type, Some(Linkage::External));

        let strcmp_type = self.context.i32_type().fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        self.module.add_function("strcmp", strcmp_type, Some(Linkage::External));

        // i8* gets(i8* s)
        let gets_type = self.context.i8_type().ptr_type(AddressSpace::default()).fn_type(&[], true);
        self.module.add_function("gets", gets_type, Some(Linkage::External));

        // i8* string_input(i32 num)
        let input_string_type = self.context.i8_type().ptr_type(AddressSpace::default()).fn_type(&[self.context.i32_type().into()], false);
        self.create_input_string_function(input_string_type);
    }

    fn create_clone_string_function(&self) -> FunctionValue<'a> {
        // Define the function signature: i8* clone_string(i8*)
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let clone_type = i8_ptr_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function("clone_string", clone_type, None);
    
        // Create the entry block and necessary blocks
        let entry_block = self.context.append_basic_block(function, "entry");
        let loop_block = self.context.append_basic_block(function, "loop");
        let after_loop_block = self.context.append_basic_block(function, "after_loop");
    
        self.builder.position_at_end(entry_block);
    
        // Get the input parameter (the original string)
        let original_str = function.get_first_param().unwrap().into_pointer_value();
    
        // Allocate space for the index
        let index = self.builder.build_alloca(self.context.i64_type(), "").unwrap();
        self.builder.build_store(index, self.context.i64_type().const_zero()).unwrap();
    
        // Calculate the length of the original string (including the null terminator)
        let length = self.builder.build_call(self.module.get_function("strlen").unwrap(), &[original_str.into()], "").unwrap().try_as_basic_value().left().unwrap();
    
        // Allocate space for the new string
        let cloned_str = self.builder.build_array_alloca(self.context.i8_type(), length.into_int_value(), "").unwrap();
    
        // Jump to the loop block
        self.builder.build_unconditional_branch(loop_block).unwrap();
    
        // Loop block: copy each character from the original string to the cloned string
        self.builder.position_at_end(loop_block);
        let current_index = self.builder.build_load(index, "").unwrap().into_int_value();
    
        // Get the current character from the original string
        let original_char_ptr = unsafe {
            self.builder.build_in_bounds_gep(original_str, &[current_index], "").unwrap()
        };
        let original_char = self.builder.build_load(original_char_ptr, "").unwrap();
    
        // Get the corresponding position in the cloned string
        let cloned_char_ptr = unsafe {
            self.builder.build_in_bounds_gep(cloned_str, &[current_index], "").unwrap()
        };
    
        // Store the character in the cloned string
        self.builder.build_store(cloned_char_ptr, original_char).unwrap();
    
        // Increment the index
        let next_index = self.builder.build_int_add(current_index, self.context.i64_type().const_int(1, false), "").unwrap();
        self.builder.build_store(index, next_index).unwrap();
    
        // Continue looping
        let is_done = self.builder
            .build_int_compare(IntPredicate::EQ, current_index, length.into_int_value(), "")
            .unwrap();
        self.builder.build_conditional_branch(is_done, after_loop_block, loop_block).unwrap();
    
        // After the loop, return the cloned string pointer
        self.builder.position_at_end(after_loop_block);
        self.builder.build_return(Some(&cloned_str)).unwrap();
    
        function
    }

    fn create_strlen_function(&self) -> FunctionValue<'a> {
        // Define the strlen function signature: i64 strlen(i8*)
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let strlen_type = self.context.i64_type().fn_type(&[i8_ptr_type.into()], false);
    
        // Add strlen function declaration to the module
        self.module.add_function("strlen", strlen_type, None)
    }

    fn create_panic_handler(&self) -> FunctionValue<'a> {
        let function  = self.create_function("panic", self.context.void_type().fn_type(&[self.context.i8_type().ptr_type(AddressSpace::default()).into(), self.context.i64_type().into()], false));

        // self.builder.build_call(self.module.get_function("printf").unwrap(), &[function.get_first_param().unwrap().into()], "").unwrap();

        let string = function.get_first_param().unwrap().as_basic_value_enum().into_pointer_value();
        let string_length = function.get_nth_param(1).unwrap().as_basic_value_enum().into_int_value();
        self.builder.build_call(self.module.get_function("syscall").unwrap(), &[
            self.context.i64_type().const_int(1, false).into(),
            self.context.i64_type().const_int(2, false).into(),
            self.builder.build_ptr_to_int(string, self.context.i64_type(), "").unwrap().into(),
            string_length.into()
        ], "").unwrap();


        self.builder.build_call(self.module.get_function("abort").unwrap(), &[], "").unwrap();

        self.builder.build_return(None).unwrap();

        function
    }

    fn create_input_string_function(&self, function_type: FunctionType<'a>) -> FunctionValue<'a> {
        let function = self.create_function("string_input", function_type);

        let num = self.builder.build_int_cast(function.get_first_param().unwrap().into_int_value(), self.context.i64_type(), "").unwrap();

        let zero_value = self.builder.build_int_compare(IntPredicate::SGE, num, self.context.i64_type().const_zero(), "").unwrap();
        let high_value = self.builder.build_int_compare(IntPredicate::SLE, num, self.context.i64_type().const_int(1023, false), "").unwrap();
        let invalid_value = self.builder.build_and(zero_value, high_value, "").unwrap();

        let then_block = self.create_basic_block("", function);
        let else_block = self.create_basic_block("", function);

        self.builder.build_conditional_branch(invalid_value, then_block, else_block).unwrap();

        self.builder.position_at_end(else_block);
        let error_string = self.builder.build_global_string_ptr("Invalid read length provided to string_input.\n", "").unwrap();
        self.builder.build_call(self.module.get_function("panic").unwrap(), &[error_string.as_pointer_value().into(), self.context.i64_type().const_int(46, false).into()], "").unwrap();
        self.builder.build_return(Some(&self.context.i8_type().ptr_type(AddressSpace::default()).const_null())).unwrap();

        self.builder.position_at_end(then_block);
        
        let file_descriptor = self.context.i64_type().const_zero();

        let buffer = self.builder.build_array_malloc(self.context.i8_type(), num, "").unwrap();
        
        let buf_ptr = self.builder.build_ptr_to_int(buffer, self.context.i64_type(), "").unwrap();
        let syscall_number = self.context.i64_type().const_zero();

        let bytes_read = self.builder.build_call(self.module.get_function("syscall").unwrap(), &[syscall_number.into(), file_descriptor.into(), buf_ptr.into(), num.into()], "").unwrap().try_as_basic_value().left().unwrap();
        
        let end = self.builder.build_int_sub(bytes_read.into_int_value(), self.context.i64_type().const_int(1, false), "").unwrap();
        let null_pos = unsafe { self.builder
            .build_in_bounds_gep(
                buffer,
                &[end],
                "",
            )
            .unwrap() };

        self.builder.build_store(null_pos, self.context.i8_type().const_zero()).unwrap();

        let casted_buf_ptr = self.builder.build_int_to_ptr(buf_ptr, self.context.i8_type().ptr_type(AddressSpace::default()), "").unwrap();
        self.builder.build_return(Some(&casted_buf_ptr)).unwrap();

        function.add_attribute(AttributeLoc::Function, self.context.create_string_attribute("allockind", "alloc"));

        function
    }

    fn create_input_int_function(&self, function_type: FunctionType<'a>) -> FunctionValue<'a> {
        let function = self.create_function("int_input", function_type);

        let format = self.builder.build_global_string_ptr("%d", "").unwrap().as_pointer_value();

        let input = self.builder.build_alloca(self.context.i32_type(), "input").unwrap();

        let params: Vec<BasicMetadataValueEnum<'a>> = vec![format.into(), input.into()];
        self.builder.build_call(self.module.get_function("scanf").unwrap(), &params, "").unwrap();

        self.builder.build_return(Some(&self.builder.build_load(input, "").unwrap())).unwrap();

        function
    }

    fn create_print_function(&self, function_type: FunctionType<'a>) -> FunctionValue<'a> {
        let function = self.create_function("print", function_type);

        let params: Vec<BasicMetadataValueEnum<'a>> = function.get_params().iter().map(|&val| val.into()).collect();
        self.builder.build_call(self.module.get_function("printf").unwrap(), &params, "").unwrap();

        let new_line = self.builder.build_global_string_ptr("\n", "").unwrap().as_pointer_value();

        self.builder.build_call(self.module.get_function("printf").unwrap(), &[new_line.into()], "").unwrap();

        self.builder.build_return(None).unwrap();

        function
    }

    pub fn create_function(&self, name: &str, function_type: FunctionType<'a>) -> FunctionValue<'a> {
        let mut function = self.module.get_function(name);

        if function.is_none() {
            function = Some(self.create_function_proto(name, function_type));
        }

        self.create_function_block(function.unwrap());

        let function = function.unwrap();

        let attributes = vec![
            self.context.create_enum_attribute(Attribute::get_named_enum_kind_id("uwtable"), 0),
            self.context.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0),
        ];

        for attribute in attributes.iter() {
            function.add_attribute(AttributeLoc::Function, *attribute);
        }

        function
    }

    fn create_function_proto(&self, name: &str, function_type: FunctionType<'a>) -> FunctionValue<'a> {
        let function = self.module.add_function(name, function_type, Some(Linkage::External));

        assert!(function.verify(true));

        function
    }

    fn create_function_block(&self, function: FunctionValue<'a>) {
        let entry = self.create_basic_block("entry", function);
        self.builder.position_at_end(entry);
    }

    fn create_basic_block(&self, name: &str, function: FunctionValue<'a>) -> BasicBlock {
        self.context.append_basic_block(function, name)
    }
}

pub fn compile(ast: TypedBlockStmt, type_checker: TypeChecker, output_file: PathBuf, file_name: &str) -> Result<(), Error> {
    let context = Context::create();
    let mut compiler = Compiler::new(false, ast, type_checker, &context, file_name);

    compiler.compile();
    compiler.save_module_to_file(output_file);

    Ok(())
}