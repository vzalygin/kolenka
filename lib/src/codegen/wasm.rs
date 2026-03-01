//! Модуль с обертками над Wasm

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, DataSection, ElementSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, ImportSection, MemorySection, Module, TableSection,
    TypeSection, ValType,
};

struct WasmModule {
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    tables: TableSection,
    memory: MemorySection,
    globals: GlobalSection,
    exports: ExportSection,
    elements: ElementSection,
    codes: CodeSection,
    data: DataSection,
    types_map: HashMap<WasmType, u32>,
    functions_map: HashMap<String, u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WasmType {
    params: Vec<ValType>,
    result: Vec<ValType>,
}

impl WasmModule {
    fn new() -> WasmModule {
        WasmModule {
            types: TypeSection::new(),
            imports: ImportSection::new(),
            functions: FunctionSection::new(),
            tables: TableSection::new(),
            memory: MemorySection::new(),
            globals: GlobalSection::new(),
            exports: ExportSection::new(),
            elements: ElementSection::new(),
            codes: CodeSection::new(),
            data: DataSection::new(),
            types_map: HashMap::new(),
            functions_map: HashMap::new(),
        }
    }

    fn type_id(&mut self, t: WasmType) -> u32 {
        *self.types_map.entry(t).or_insert_with_key(|k| {
            self.types.ty().function(k.params.clone(), k.result.clone());
            self.types.len() - 1
        })
    }

    fn function_id<'a>(&mut self, name: impl Into<&'a str>) -> Option<u32> {
        let name = name.into();

        self.functions_map.get(name).copied()
    }

    fn import_function<'a>(
        &mut self,
        module: impl Into<&'a str>,
        name: impl Into<&'a str>,
        t: WasmType,
    ) -> u32 {
        let module = module.into();
        let name = name.into();

        let id = self.type_id(t);
        self.imports.import(module, name, EntityType::Function(id));

        id
    }

    fn function<'a>(&mut self, t: WasmType, definition: Function) -> u32 {
        let id = self.type_id(t);
        self.functions.function(id);
        self.codes.function(&definition);

        id
    }

    fn function_exported<'a>(
        &mut self,
        name: impl Into<&'a str>,
        t: WasmType,
        definition: Function,
    ) -> u32 {
        let id = self.function(t, definition);
        let name = name.into();

        self.exports.export(name, ExportKind::Func, id);

        id
    }

    fn finish(self) -> Vec<u8> {
        let mut module = Module::new();

        module.section(&self.types);
        module.section(&self.imports);
        module.section(&self.functions);
        module.section(&self.tables);
        module.section(&self.memory);
        module.section(&self.globals);
        module.section(&self.exports);
        module.section(&self.elements);
        module.section(&self.codes);
        module.section(&self.data);

        module.finish()
    }
}

impl WasmType {
    fn new(params: impl Into<Vec<ValType>>, result: impl Into<Vec<ValType>>) -> WasmType {
        WasmType {
            params: params.into(),
            result: result.into(),
        }
    }
}
