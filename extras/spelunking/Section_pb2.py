# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: Section.proto

import sys
_b=sys.version_info[0]<3 and (lambda x:x) or (lambda x:x.encode('latin1'))
from google.protobuf.internal import enum_type_wrapper
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()


import ByteInterval_pb2 as ByteInterval__pb2


DESCRIPTOR = _descriptor.FileDescriptor(
  name='Section.proto',
  package='gtirb.proto',
  syntax='proto3',
  serialized_options=_b('\n\032com.grammatech.gtirb.proto'),
  serialized_pb=_b('\n\rSection.proto\x12\x0bgtirb.proto\x1a\x12\x42yteInterval.proto\"\xa4\x01\n\x07Section\x12\x0c\n\x04uuid\x18\x01 \x01(\x0c\x12\x0c\n\x04name\x18\x02 \x01(\t\x12\x31\n\x0e\x62yte_intervals\x18\x05 \x03(\x0b\x32\x19.gtirb.proto.ByteInterval\x12/\n\rsection_flags\x18\x06 \x03(\x0e\x32\x18.gtirb.proto.SectionFlagJ\x04\x08\x03\x10\x04J\x04\x08\x04\x10\x05R\x07\x61\x64\x64ressR\x04size*~\n\x0bSectionFlag\x12\x15\n\x11Section_Undefined\x10\x00\x12\x0c\n\x08Readable\x10\x01\x12\x0c\n\x08Writable\x10\x02\x12\x0e\n\nExecutable\x10\x03\x12\n\n\x06Loaded\x10\x04\x12\x0f\n\x0bInitialized\x10\x05\x12\x0f\n\x0bThreadLocal\x10\x06\x42\x1c\n\x1a\x63om.grammatech.gtirb.protob\x06proto3')
  ,
  dependencies=[ByteInterval__pb2.DESCRIPTOR,])

_SECTIONFLAG = _descriptor.EnumDescriptor(
  name='SectionFlag',
  full_name='gtirb.proto.SectionFlag',
  filename=None,
  file=DESCRIPTOR,
  values=[
    _descriptor.EnumValueDescriptor(
      name='Section_Undefined', index=0, number=0,
      serialized_options=None,
      type=None),
    _descriptor.EnumValueDescriptor(
      name='Readable', index=1, number=1,
      serialized_options=None,
      type=None),
    _descriptor.EnumValueDescriptor(
      name='Writable', index=2, number=2,
      serialized_options=None,
      type=None),
    _descriptor.EnumValueDescriptor(
      name='Executable', index=3, number=3,
      serialized_options=None,
      type=None),
    _descriptor.EnumValueDescriptor(
      name='Loaded', index=4, number=4,
      serialized_options=None,
      type=None),
    _descriptor.EnumValueDescriptor(
      name='Initialized', index=5, number=5,
      serialized_options=None,
      type=None),
    _descriptor.EnumValueDescriptor(
      name='ThreadLocal', index=6, number=6,
      serialized_options=None,
      type=None),
  ],
  containing_type=None,
  serialized_options=None,
  serialized_start=217,
  serialized_end=343,
)
_sym_db.RegisterEnumDescriptor(_SECTIONFLAG)

SectionFlag = enum_type_wrapper.EnumTypeWrapper(_SECTIONFLAG)
Section_Undefined = 0
Readable = 1
Writable = 2
Executable = 3
Loaded = 4
Initialized = 5
ThreadLocal = 6



_SECTION = _descriptor.Descriptor(
  name='Section',
  full_name='gtirb.proto.Section',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  fields=[
    _descriptor.FieldDescriptor(
      name='uuid', full_name='gtirb.proto.Section.uuid', index=0,
      number=1, type=12, cpp_type=9, label=1,
      has_default_value=False, default_value=_b(""),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='name', full_name='gtirb.proto.Section.name', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=_b("").decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='byte_intervals', full_name='gtirb.proto.Section.byte_intervals', index=2,
      number=5, type=11, cpp_type=10, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='section_flags', full_name='gtirb.proto.Section.section_flags', index=3,
      number=6, type=14, cpp_type=8, label=3,
      has_default_value=False, default_value=[],
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
  serialized_start=51,
  serialized_end=215,
)

_SECTION.fields_by_name['byte_intervals'].message_type = ByteInterval__pb2._BYTEINTERVAL
_SECTION.fields_by_name['section_flags'].enum_type = _SECTIONFLAG
DESCRIPTOR.message_types_by_name['Section'] = _SECTION
DESCRIPTOR.enum_types_by_name['SectionFlag'] = _SECTIONFLAG
_sym_db.RegisterFileDescriptor(DESCRIPTOR)

Section = _reflection.GeneratedProtocolMessageType('Section', (_message.Message,), dict(
  DESCRIPTOR = _SECTION,
  __module__ = 'Section_pb2'
  # @@protoc_insertion_point(class_scope:gtirb.proto.Section)
  ))
_sym_db.RegisterMessage(Section)


DESCRIPTOR._options = None
# @@protoc_insertion_point(module_scope)
