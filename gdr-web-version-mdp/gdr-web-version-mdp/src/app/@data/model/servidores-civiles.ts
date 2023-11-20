export class ServidoresCiviles {
  detUnidadOrganicaId: number;
  personaId: number;
  organigramaId: number;
  docIdentidad: string;
  apellidosNombres: string;
  uoArea: string;
  puesto: string;
  estado: string;
  unidadOrganica: string;
  estadoId?: string;
  settings?: any;
}


export interface DatosPersonalesServidorCivil {
  detalleuoId?: number;
  uoId?: number;
  personaId?: number;
  apellidoPaterno?: string;
  apellidoMaterno?: string;
  mombres?: string;
  tipoDocumento?: string;
  numeroDocumento?: string;
  telefono?: string;
  genero?: string;
  fechaNacimiento?: Date;
  correoInstitucional?: string;
  correoAlternativo?: string;
  regimenLaboral?: string;
  sindicato?: string;
  tipoAsignacion?: string;
  puestoId?: number;
  puestoDescripcion?: string;
  fechaInicio?: Date;
}

export class ParticipantesCiviles {
  detUnidadOrganicaId?: number;
  personaId?: number;
  documentoIdentidad?: string;
  numeroDocumento?: string;
  apellidosNombres?: string;
  unidadOrganicaId?: number;
  esJefeUoId?: number;
  esJefeUoDesc?: string;
  puesto?: string;
  tipoAsignacion?: string;
  segmentoId?: number;
  segmento?: string;
  descripcionRoles?: string;
  rolId?: number;
  estadoSerCivGdrId?: number;
  estado?: string;
  siglaUO?: string;

  flagCheck?: boolean;
  estadoId?: string;
  settings?: any;
roles?: any[];
}
