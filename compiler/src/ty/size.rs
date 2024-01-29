use crate::{
    db::{AdtKind, Db, StructKind},
    target::TargetMetrics,
    ty::{fold::TyFolder, FloatTy, IntTy, TyKind, UintTy},
};

// Sizes here must be synced with the runtime types!
impl TyKind {
    pub fn size(&self, db: &Db) -> usize {
        let target_metrics = db.target_metrics();
        let word = target_metrics.word_size;

        match self {
            TyKind::Int(x) => x.size(target_metrics),
            TyKind::Uint(x) => x.size(target_metrics),
            TyKind::Float(x) => x.size(),
            TyKind::Fn(_) => word * 2,
            TyKind::Ref(inner, _) => inner.size(db),
            TyKind::RawPtr(_) => word,
            TyKind::Slice(_) | TyKind::Str => word * 4,
            TyKind::Adt(adt_id, targs) => {
                let adt = &db[*adt_id];

                match &adt.kind {
                    AdtKind::Struct(sdef) => match &sdef.kind {
                        StructKind::Ref => word,
                        StructKind::Extern => {
                            let instantiation = adt.instantiation(targs);
                            let mut folder = instantiation.folder();
                            let mut offset = 0;

                            for field in sdef.fields.iter() {
                                let field_ty = folder.fold(field.ty);
                                let align = field_ty.align(db);
                                offset = calculate_align_from_offset(offset, align);
                                offset += field_ty.size(db);
                            }

                            offset = calculate_align_from_offset(offset, self.align(db));

                            offset
                        }
                    },
                    AdtKind::Union(_) => word,
                }
            }
            TyKind::Bool => 1,
            TyKind::Unit | TyKind::Never => 0,
            TyKind::Param(_)
            | TyKind::Infer(_)
            | TyKind::Type(_)
            | TyKind::Module(_)
            | TyKind::Unknown => panic!("cant find size of {self:?}"),
        }
    }

    pub fn align(&self, db: &Db) -> usize {
        match self {
            TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Fn(_)
            | TyKind::Ref(_, _)
            | TyKind::RawPtr(_)
            | TyKind::Slice(_)
            | TyKind::Str => self.size(db),
            TyKind::Adt(adt_id, targs) => {
                let adt = &db[*adt_id];

                match &adt.kind {
                    AdtKind::Struct(sdef) => match &sdef.kind {
                        StructKind::Ref => db.target_metrics().word_size,
                        StructKind::Extern => {
                            let instantiation = adt.instantiation(targs);
                            let mut folder = instantiation.folder();
                            let mut max_align: usize = 1;

                            for field in sdef.fields.iter() {
                                let field_ty = folder.fold(field.ty);
                                let field_align = field_ty.align(db);
                                max_align = max_align.max(field_align);
                            }
                            max_align
                        }
                    },
                    AdtKind::Union(_) => db.target_metrics().word_size,
                }
            }
            TyKind::Bool | TyKind::Unit | TyKind::Never => 1,
            TyKind::Param(_)
            | TyKind::Infer(_)
            | TyKind::Type(_)
            | TyKind::Module(_)
            | TyKind::Unknown => panic!("cant find size of {self:?}"),
        }
    }
}

impl IntTy {
    pub fn size(self, target_metrics: &TargetMetrics) -> usize {
        match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::Int => target_metrics.word_size,
        }
    }

    pub fn align(self, target_metrics: &TargetMetrics) -> usize {
        self.size(target_metrics)
    }
}

impl UintTy {
    pub fn size(self, target_metrics: &TargetMetrics) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::Uint => target_metrics.word_size,
        }
    }

    pub fn align(self, target_metrics: &TargetMetrics) -> usize {
        self.size(target_metrics)
    }
}

impl FloatTy {
    pub fn size(self) -> usize {
        match self {
            Self::F32 => 4,
            Self::F64 => 8,
        }
    }

    pub fn align(self) -> usize {
        self.size()
    }
}

fn bit_width_to_size(w: u32) -> usize {
    ((w + 7) / 8) as usize
}

fn calculate_align(size: usize, align: usize) -> usize {
    if align > 0 {
        let res = size + align - 1;
        res - res % align
    } else {
        size
    }
}

fn calculate_align_from_offset(offset: usize, align: usize) -> usize {
    (offset + align - 1) / align * align
}
