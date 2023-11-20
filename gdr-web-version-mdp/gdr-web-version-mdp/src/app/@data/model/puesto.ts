export class Puesto {
  "id"?: number;
  "descripcion"?: string;

}

export class Puestos {
  "correlativo": number;
  "detalleuoId": number;
  "siglaUO": string;
  "puestoId": number;
  "puesto": string;
  "tipoAsignacion": string;
  "fechaInicio": string;
  "fechaCese": string;
  "segmento": string;
  "rol": string;
  "estado": string;
}


export class PuestoUoServidorCivil {
  uoId?: number;
  detuoId?: number;
  personaId?: number;
  entidadId?: number;
  correlativo?: number;
  detalleuoId?: number;
  siglaUO?: string;
  puestoId?: number;
  puesto?: string;
  tipoAsignacion?: string;
  personaIdAsignada?: number;
  fechaInicio?: Date;
  fechaCese?: Date;
  fechaInicioTexto?: string;
  fechaCeseTexto?: string;
  segmento?: string;
  rol?: string;
  estado?: string;
}
