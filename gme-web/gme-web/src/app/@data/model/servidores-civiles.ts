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
  colorEstado?: string;
  regimenId?: number;
}


export interface DatosPersonalesServidorCivil {
  detalleuoId?: number;
  uoId?: number;
  personaId?: number;
  apellidoPaterno?: string;
  apellidoMaterno?: string;
  nombres?: string;
  tipoDocumento?: string;
  numeroDocumento?: string;
  telefono?: string;
  genero?: string;
  fechaNacimiento?: Date;
  correoInstitucional?: string;
  correoAlternativo?: string;
  regimenLaboral?: string;
  regimenLaboralId?: number;
  sindicato?: string;
  tipoAsignacion?: string;
  puestoId?: number;
  puestoDescripcion?: string;
  fechaInicio?: Date;
  regimenId?: number;
}
