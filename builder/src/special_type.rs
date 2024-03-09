use syn::{AngleBracketedGenericArguments, GenericArgument, PathArguments, PathSegment, Type};

fn get_first_generic_type(args: &AngleBracketedGenericArguments) -> Option<&Type> {
    args.args.first().and_then(|arg| match arg {
        GenericArgument::Type(ty) => Some(ty),
        _ => None,
    })
}

fn first_generic_type(segment: &PathSegment) -> Option<&Type> {
    match &segment.arguments {
        PathArguments::AngleBracketed(args) => get_first_generic_type(args),
        _ => None,
    }
}

fn last_segment(ty: &Type) -> Option<&PathSegment> {
    match ty {
        Type::Path(type_path) => type_path
            .path
            .segments
            .last(),
        _ => None,
    }
}

fn option_inner_type_from_ident(segment: &PathSegment) -> Option<&Type> {
    if segment.ident == "Option" {
        first_generic_type(segment)
    } else {
        None
    }
}

fn vec_inner_type_from_ident(segment: &PathSegment) -> Option<&Type> {
    if segment.ident == "Vec" {
        first_generic_type(segment)
    } else {
        None
    }
}

pub fn option_inner_type(ty: &Type) -> Option<&Type> {
    last_segment(ty).and_then(option_inner_type_from_ident)
}

pub fn vec_inner_type(ty: &Type) -> Option<&Type> {
    last_segment(ty).and_then(vec_inner_type_from_ident)
}
