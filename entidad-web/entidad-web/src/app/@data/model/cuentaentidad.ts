export class CuentaEntidad {
  cuentaId?: number;
  fechaAlta?: string;
  fechaBaja?: string;
  nombreRol?: string;
  nombres?: string;
  correo?: string;
  apellidoPaterno?: string;
  apellidoMaterno?: string;
  descripcionPuesto?: string;
  estadoId?: number;
  descripcionEstado: string;
  apellidos?: string;
  nombreCompleto?: string;
  listaRoles: Rol[] = [];
  rol?: string;
  rolId?: number;
  numeroTelefono?: number;
  anexo?: number;
  tipoDocumento?: number;
  tipoDocumentoTexto?: string;
  nroDocumento?: number;
  flagCuentaEditable?: string;
  editable: boolean;
  personaId?: number;
  paisId?: number;
  pais?: string;
  telefonoId?: number;
  correoId?: number;
  usuarioId?: number;
}

export class Rol {
  cuentaId?: number;
  descripcionEstado?: string;
  estadoId?: number;
  fechaAltaRol?: Date;
  fechaBajaRol?: Date;
  nombreRol?: string;
  nroRoles?: number;
  usuarioId?: number;
  rolId?: number;
  usuarioRolId?: number;
  editable: boolean;
}

export class Trace {
  traceId?: any;
}

export class Error {
  code?: any;
  httpCode?: any;
  messages: any[];
}

export class Status {
  success: boolean;
  error: Error;
}

export class Payload {
  listaEntidades: CuentaEntidad[];
}

export class CuetaEntidadResponse {
  trace: Trace;
  status: Status;
  payload: Payload;
}

export class CuentaEntidadRequest {
  typeDocument?: number;
  numberDocument?: number;
  name?: string;
  fatherName?: string;
  motherName?: string;
  email?: string;
  phone?: number;
  anexo?: number;
  personaId?: number;
  country?: number;
  puesto?: string;
  stateCuentas?: string;
  commet?: string;
  rol?: string[];
  telefonoId?: number;
  correoId?: number;
  cuentaId?: number;
  usuarioId?: number;
  roles?: Rol[];
}
/*
cuentas-asociadas.component.ts:325 this.f.numberDocument.value
cuentas-asociadas.component.ts:326 72901060
cuentas-asociadas.component.ts:327 this.f.name.value
cuentas-asociadas.component.ts:328 PEDRO EMILIO AXEL
cuentas-asociadas.component.ts:329 this.f.fatherName.value
cuentas-asociadas.component.ts:330 ZEVALLOS
cuentas-asociadas.component.ts:331 this.f.motherName.value
cuentas-asociadas.component.ts:332 GARCIA
cuentas-asociadas.component.ts:333 this.f.email.value
cuentas-asociadas.component.ts:334 azevallos0815@gmail.com
cuentas-asociadas.component.ts:335 this.f.phone.value
cuentas-asociadas.component.ts:336 123123123
cuentas-asociadas.component.ts:337 this.f.anexo.value
cuentas-asociadas.component.ts:338
cuentas-asociadas.component.ts:339 this.f.country.value
cuentas-asociadas.component.ts:340
cuentas-asociadas.component.ts:341 this.f.countryObject.value
cuentas-asociadas.component.ts:342
cuentas-asociadas.component.ts:343 this.f.puesto.value
cuentas-asociadas.component.ts:344 Gerente
cuentas-asociadas.component.ts:345 this.f.stateCuentas.value
cuentas-asociadas.component.ts:346 14
cuentas-asociadas.component.ts:347 this.f.rol.value
cuentas-asociadas.component.ts:348 (2) [99, 100]
 */
