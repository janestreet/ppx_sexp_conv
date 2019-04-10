open! Base
open! Ppxlib

val default : (label_declaration, expression) Attribute.t

val drop_default : (label_declaration, expression option) Attribute.t

val drop_default_equal : (label_declaration, unit) Attribute.t

val drop_default_compare : (label_declaration, unit) Attribute.t

val drop_default_sexp : (label_declaration, unit) Attribute.t

val drop_if : (label_declaration, expression) Attribute.t

val opaque : (core_type, unit) Attribute.t

val omit_nil : (label_declaration, unit) Attribute.t

val option : (label_declaration, unit) Attribute.t

val list : (label_declaration, unit) Attribute.t

val array : (label_declaration, unit) Attribute.t

val bool : (label_declaration, unit) Attribute.t

val list_variant : (constructor_declaration, unit) Attribute.t

val list_extension : (extension_constructor, unit) Attribute.t

val list_poly : (row_field, unit) Attribute.t

val allow_extra_fields_td : (type_declaration, unit) Attribute.t

val allow_extra_fields_cd : (constructor_declaration, unit) Attribute.t

val invalid_attribute : loc:Location.t -> (_, _) Attribute.t -> string -> 'a

val fail_if_allow_extra_field_cd : loc:Location.t -> constructor_declaration -> unit

val fail_if_allow_extra_field_td : loc:Location.t -> type_declaration -> unit
