# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: AuxData.proto

import sys
_b=sys.version_info[0]<3 and (lambda x:x) or (lambda x:x.encode('latin1'))
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()




DESCRIPTOR = _descriptor.FileDescriptor(
  name='AuxData.proto',
  package='gtirb.proto',
  syntax='proto3',
  serialized_options=_b('\n\032com.grammatech.gtirb.proto'),
  serialized_pb=_b('\n\rAuxData.proto\x12\x0bgtirb.proto\"*\n\x07\x41uxData\x12\x11\n\ttype_name\x18\x01 \x01(\t\x12\x0c\n\x04\x64\x61ta\x18\x02 \x01(\x0c\x42\x1c\n\x1a\x63om.grammatech.gtirb.protob\x06proto3')
)




_AUXDATA = _descriptor.Descriptor(
  name='AuxData',
  full_name='gtirb.proto.AuxData',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  fields=[
    _descriptor.FieldDescriptor(
      name='type_name', full_name='gtirb.proto.AuxData.type_name', index=0,
      number=1, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=_b("").decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='data', full_name='gtirb.proto.AuxData.data', index=1,
      number=2, type=12, cpp_type=9, label=1,
      has_default_value=False, default_value=_b(""),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=30,
  serialized_end=72,
)

DESCRIPTOR.message_types_by_name['AuxData'] = _AUXDATA
_sym_db.RegisterFileDescriptor(DESCRIPTOR)

AuxData = _reflection.GeneratedProtocolMessageType('AuxData', (_message.Message,), dict(
  DESCRIPTOR = _AUXDATA,
  __module__ = 'AuxData_pb2'
  # @@protoc_insertion_point(class_scope:gtirb.proto.AuxData)
  ))
_sym_db.RegisterMessage(AuxData)


DESCRIPTOR._options = None
# @@protoc_insertion_point(module_scope)