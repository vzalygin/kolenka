//! Модуль с обертками над Wasm

use std::{collections::HashMap, fs::File, io::Write};

use derived_deref::{Deref, DerefMut};
use wasm_encoder::{
    CodeSection, DataSection, ElementSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, ImportSection, MemorySection, Module, TableSection,
    TypeSection, ValType,
};

const STD_MODULE: &str = "kolenka_std";
const STD_READ_I32: &str = "read_i32";
const STD_PRINT_I32: &str = "print_i32";

#[derive(Debug, Clone, Copy, Deref, DerefMut)]
pub(crate) struct WasmTypeId(u32);

impl WasmTypeId {
    fn new(id: u32) -> WasmTypeId {
        WasmTypeId(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deref, DerefMut)]
pub(crate) struct WasmLocalId(u32);

impl WasmLocalId {
    pub(crate) fn new(id: u32) -> WasmLocalId {
        WasmLocalId(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deref, DerefMut)]
pub(crate) struct WasmFunctionId(u32);

impl WasmFunctionId {
    pub(crate) fn new(id: u32) -> WasmFunctionId {
        WasmFunctionId(id)
    }
}

pub struct WasmModule {
    // pregenerated sections
    types: TypeSection,
    // functions: FunctionSection,
    // codes: CodeSection,
    tables: TableSection,
    memory: MemorySection,
    globals: GlobalSection,
    elements: ElementSection,
    data: DataSection,
    imports: ImportSection,
    exports: ExportSection,

    // types
    // TODO тут использовать WasmTypeId
    types_ids: HashMap<WasmType, u32>,

    // functions
    functions_decls: HashMap<WasmFunctionId, WasmTypeId>,
    functions_defs: HashMap<WasmFunctionId, Function>,
}

impl WasmModule {
    pub(crate) fn new() -> WasmModule {
        WasmModule {
            types: TypeSection::new(),
            imports: ImportSection::new(),
            tables: TableSection::new(),
            memory: MemorySection::new(),
            globals: GlobalSection::new(),
            exports: ExportSection::new(),
            elements: ElementSection::new(),
            data: DataSection::new(),

            types_ids: HashMap::new(),

            functions_decls: HashMap::new(),
            functions_defs: HashMap::new(),
        }
    }

    pub(crate) fn function_declaration(&mut self, t: WasmType) -> WasmFunctionId {
        let type_id = self.type_id(t);
        let function_id = WasmFunctionId::new(self.functions_decls.len() as u32 + self.imports.len());
        self.functions_decls.insert(function_id, type_id);

        function_id
    }

    pub(crate) fn function_definition(
        &mut self,
        id: WasmFunctionId,
        definition: Function
    ) {
        self.functions_defs.insert(id, definition);
    }

    /// Импорты должны быть строго до первых собственных определений
    pub(crate) fn import_function<'a>(
        &mut self,
        module: impl Into<&'a str>,
        name: impl Into<&'a str>,
        t: WasmType,
    ) -> WasmFunctionId {
        let module = module.into();
        let name = name.into();

        let type_id = self.type_id(t);
        let function_id = WasmFunctionId::new(self.imports.len());
        self.imports.import(module, name, EntityType::Function(type_id.0));
        
        function_id
    }

    pub(crate) fn export_function<'a>(
        &mut self,
        id: WasmFunctionId,
        name: impl Into<&'a str>,
    ) {
        self.exports.export(name.into(), ExportKind::Func, *id);
    }

    pub fn finish(self) -> Vec<u8> {
        let mut module = Module::new();

        let mut function_section = FunctionSection::new();
        let mut code_section = CodeSection::new();
        for function_id in 0..self.functions_decls.len() {
            // сдвиг индекса от импортированных функций
            let function_id = WasmFunctionId::new(function_id as u32 + self.imports.len());

            let function_type = self.functions_decls[&function_id];
            let function_code = &self.functions_defs[&function_id];

            function_section.function(*function_type);
            code_section.function(function_code);
        }

        // порядок важен
        module.section(&self.types);
        module.section(&self.imports);
        module.section(&function_section);
        module.section(&self.tables);
        module.section(&self.memory);
        module.section(&self.globals);
        module.section(&self.exports);
        module.section(&self.elements);
        module.section(&code_section);
        module.section(&self.data);
    
        module.finish()
    }

    fn type_id(&mut self, t: WasmType) -> WasmTypeId {
        WasmTypeId::new(*self.types_ids.entry(t).or_insert_with_key(|k| {
            self.types.ty().function(k.args.clone(), k.rets.clone());
            self.types.len() - 1
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct WasmType {
    pub(crate) args: Vec<ValType>,
    pub(crate) rets: Vec<ValType>,
}

impl WasmType {
    fn new(params: impl Into<Vec<ValType>>, result: impl Into<Vec<ValType>>) -> WasmType {
        WasmType {
            args: params.into(),
            rets: result.into(),
        }
    }
}

pub(crate) fn module() {
    let mut m = WasmModule::new();

    let read_i32 = m.import_function(STD_MODULE, STD_READ_I32, WasmType::new([], [ValType::I32]));
    let print_i32 = m.import_function(STD_MODULE, STD_PRINT_I32, WasmType::new([ValType::I32], []));

    let inner_id = m.function_declaration(WasmType::new([ValType::I32], [ValType::I32]));
    let start_id = m.function_declaration(WasmType::new([], []));

    let mut inner = Function::new([(1, ValType::I32)]);
    inner
        .instructions()
        .local_get(0)
        .local_set(1)
        .local_get(1)
        .local_get(1)
        .i32_add()
        .return_();
    m.function_definition(inner_id, inner);

    let mut start = Function::new([]);
    start
        .instructions()
        .i32_const(42)
        .call(*inner_id)
        .call(*print_i32)
        .return_();
    m.function_definition(start_id, start);
    m.export_function(start_id, "_start");

    let wasm_bytes = m.finish();

    let mut file = File::create("test.wasm").unwrap();
    file.write_all(&wasm_bytes).unwrap();
}
