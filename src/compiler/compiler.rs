use std::{collections::HashMap, fmt::Error, path::PathBuf};

use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{FunctionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    ast::{
        ast::{Type, TypeType, TypeWrapper},
        types::{Literals, NumberType},
    },
    type_checker::{
        type_checker::TypeChecker,
        typed_ast::{TypedBlockStmt, TypedStmt, TypedStmtWrapper},
    },
};

use super::stmt::gen_statement;

/// The main compiler structure that holds the state of the compilation process.
pub struct Compiler<'a> {
    pub is_stdlib: bool,

    pub ast: TypedBlockStmt,
    pub type_checker: TypeChecker,
    pub current_environment: i32,

    pub named_allocas: HashMap<String, PointerValue<'a>>,
    pub named_structs: HashMap<String, StructType<'a>>,

    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(
        is_stdlib: bool,
        ast: TypedBlockStmt,
        type_checker: TypeChecker,
        context: &'a Context,
        file_name: &str,
    ) -> Self {
        Compiler {
            is_stdlib,
            ast,
            type_checker,
            current_environment: 0,
            module: context.create_module(file_name),
            builder: context.create_builder(),
            context,
            named_allocas: HashMap::new(),
            named_structs: HashMap::new(),
        }
    }

    /// Saves the current LLVM module to a file.
    pub fn save_module_to_file(&self, output_file: PathBuf) {
        self.module.print_to_file(output_file).unwrap();
        // println!("{}", self.module.print_to_string().to_str().unwrap());
    }

    /// Runs optimization passes on the LLVM module.
    fn run_passes(&self) {
        self.save_module_to_file(PathBuf::from("build/before_opt.ll"));

        let fpm = PassManager::create(());

        // fpm.add_instruction_combining_pass();
        // fpm.add_reassociate_pass(); // Reorder expressions to enable better optimizations
        // fpm.add_gvn_pass(); // Eliminate redundant calculations
        // fpm.add_cfg_simplification_pass(); // Simplify the control flow graph
        // fpm.add_basic_alias_analysis_pass(); // Analyze memory accesses
        // fpm.add_promote_memory_to_register_pass(); // Promote stack allocations to registers
        // fpm.add_merge_functions_pass(); // Merge duplicate functions
        // fpm.add_aggressive_dce_pass(); // Aggressively eliminate dead code
        // fpm.add_instruction_simplify_pass(); // Simplify instructions
        // fpm.add_dead_arg_elimination_pass(); // Eliminate unused function arguments
        // fpm.add_dead_store_elimination_pass(); // Eliminate unused stores
        // fpm.add_partially_inline_lib_calls_pass(); // Partially inline library calls
        // fpm.add_ind_var_simplify_pass(); // Simplify induction variables
        // fpm.add_loop_unroll_pass(); // Unroll loops
        // fpm.add_loop_unswitch_pass(); // Unswitch loops
        // fpm.add_loop_vectorize_pass(); // Vectorize loops
        // fpm.add_loop_deletion_pass(); // Delete dead loops
        // fpm.add_memcpy_optimize_pass(); // Eliminate redundant memcpy calls
        // fpm.add_constant_merge_pass(); // Eliminate duplicate constants
        // fpm.add_strip_symbol_pass(); // Strip symbol information
        fpm.add_verifier_pass(); // Verify the module's correctness

        fpm.run_on(&self.module);
    }

    /// The main compilation function that sets up the target machine and generates code.
    fn compile(&mut self) {
        Target::initialize_all(&InitializationConfig::default()); // Initialize all targets, targets info, target MCs, asm parsers and printers
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
        self.module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());

        self.create_external_functions();

        // Create the main function
        self.create_function("main", self.context.i32_type().fn_type(&[], false));

        // Generate the IR for the AST
        self.gen();

        // Return 0 from main
        self.builder
            .build_return(Some(&self.context.i32_type().const_zero()))
            .unwrap();

        self.run_passes();
    }

    /// Generates LLVM IR from the AST.
    fn gen(&mut self) {
        self.current_environment = self.ast.id;
        self.type_checker.clear_environment_path();
        self.type_checker
            .set_current_environment(self.current_environment);

        let statements = self
            .ast
            .body
            .iter()
            .map(|stmt| stmt.clone_typed_wrapper())
            .collect::<Vec<TypedStmtWrapper>>();
        for statement in statements.iter() {
            // Generate code for each statement in the AST
            gen_statement(self, statement);
        }
    }

    /// Converts a custom type to an LLVM basic type.
    pub fn convert_type(&self, type_: TypeType) -> BasicTypeEnum<'a> {
        match type_ {
            TypeType::Symbol(_) => {
                panic!("Attempted to convert a symbol type to a basic type");
            }
            TypeType::Array(_) => self
                .context
                .i32_type()
                .ptr_type(AddressSpace::default())
                .into(),
            TypeType::Literal(Literals::String) => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            TypeType::Literal(Literals::Number(NumberType::Int32)) => {
                self.context.i32_type().into()
            }
            TypeType::Literal(Literals::Number(NumberType::Int64)) => {
                self.context.i64_type().into()
            }
            TypeType::Literal(Literals::Number(NumberType::Int8)) => self.context.i8_type().into(),
            TypeType::Literal(Literals::Number(NumberType::Int16)) => {
                self.context.i16_type().into()
            }
            TypeType::Literal(Literals::Boolean) => self.context.bool_type().into(),
            TypeType::Literal(Literals::Null) => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            TypeType::Literal(Literals::InternalI8Pointer) => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            TypeType::Struct(name) => {
                let struct_type = self.named_structs.get(&name).unwrap();
                struct_type.ptr_type(AddressSpace::default()).into()
            }
            _ => {
                println!("{:?}", type_);
                todo!()
            }
        }
    }

    /// Declares standard library functions in the module.
    fn declare_stdlib_functions(&self, stdlib_functions: Vec<(String, TypeWrapper)>) {
        for function in stdlib_functions {
            let converted_function = function
                .1
                .as_any()
                .downcast_ref::<crate::ast::types::FunctionType>()
                .unwrap();

            let mut converted_params = vec![];
            for param in &converted_function.arguments {
                converted_params.push(self.convert_type(param.1.get_type_type()).into());
            }

            let function_type = self
                .convert_type(converted_function.return_type.get_type_type())
                .fn_type(&converted_params, false);

            self.module
                .add_function(&function.0, function_type, Some(Linkage::External));
        }
    }

    /// Creates external functions like printf and abort.
    /// Most of these should be handled by the stdlib, these are just for low-level operations.
    fn create_external_functions(&self) {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());

        let printf_type = self.context.i32_type().fn_type(&[i8_ptr_type.into()], true);
        self.module
            .add_function("printf", printf_type, Some(Linkage::External));

        let abort_type = self.context.void_type().fn_type(&[], false);
        self.module
            .add_function("abort", abort_type, Some(Linkage::External));

        self.create_panic_handler();

        // i8* strcat(i8* dest, i8* src)
        let strcat_type = self
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        self.module
            .add_function("strcat", strcat_type, Some(Linkage::External));

        let strcmp_type = self
            .context
            .i32_type()
            .fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        self.module
            .add_function("strcmp", strcmp_type, Some(Linkage::External));

        let strlen_type = self
            .context
            .i64_type()
            .fn_type(&[i8_ptr_type.into()], false);
        self.module
            .add_function("strlen", strlen_type, Some(Linkage::External));
    }

    fn create_panic_handler(&self) -> FunctionValue<'a> {
        let function = self.create_function(
            "panic",
            self.context.void_type().fn_type(
                &[
                    self.context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .into(),
                    self.context.i64_type().into(),
                ],
                false,
            ),
        );

        self.builder
            .build_call(
                self.module.get_function("printf").unwrap(),
                &[function.get_first_param().unwrap().into()],
                "",
            )
            .unwrap();

        self.builder
            .build_call(self.module.get_function("abort").unwrap(), &[], "")
            .unwrap();

        self.builder.build_return(None).unwrap();

        function
    }

    /// Creates a new function in the module.
    pub fn create_function(
        &self,
        name: &str,
        function_type: FunctionType<'a>,
    ) -> FunctionValue<'a> {
        let mut function = self.module.get_function(name);

        if function.is_none() {
            function = Some(self.create_function_proto(name, function_type));
        }

        self.create_function_block(function.unwrap());

        let function = function.unwrap();

        // Add function attributes
        let attributes = [
            self.context
                .create_enum_attribute(Attribute::get_named_enum_kind_id("uwtable"), 0),
            self.context
                .create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0),
        ];

        for attribute in attributes.iter() {
            function.add_attribute(AttributeLoc::Function, *attribute);
        }

        function
    }

    /// Creates the function prototype in the module.
    fn create_function_proto(
        &self,
        name: &str,
        function_type: FunctionType<'a>,
    ) -> FunctionValue<'a> {
        let function = self
            .module
            .add_function(name, function_type, Some(Linkage::External));

        function
    }

    /// Creates the entry basic block for a function.
    fn create_function_block(&self, function: FunctionValue<'a>) {
        let entry = self.create_basic_block("entry", function);
        self.builder.position_at_end(entry);
    }

    /// Creates a new basic block in the given function.
    fn create_basic_block(&self, name: &str, function: FunctionValue<'a>) -> BasicBlock<'a> {
        self.context.append_basic_block(function, name)
    }
}

/// The main compile function that takes in the AST, type checker, and other parameters to produce a compiled module.
///
/// Returns a Result containing the Compiler instance or an Error.
pub fn compile<'a>(
    ast: TypedBlockStmt,
    type_checker: TypeChecker,
    stdlib_functions: Vec<(String, TypeWrapper)>,
    output_file: PathBuf,
    file_name: &str,
    context: &'a Context,
) -> Result<Compiler<'a>, Error> {
    let mut compiler = Compiler::new(false, ast, type_checker, context, file_name);

    compiler.declare_stdlib_functions(stdlib_functions);

    compiler.compile();
    compiler.save_module_to_file(output_file);

    Ok(compiler)
}
