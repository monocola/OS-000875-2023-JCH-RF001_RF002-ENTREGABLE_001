export interface ICombo {
  id: number;
  codigo: number;
  codigoStr: null;
  descripcion: string;
}

export interface IEvaluaciones {
  correlativo: number;
  convocatoria: string;
  cantPerfiles: number;
  cantGrupos: number;
  fechaEvaluacion: string;
  evaluaciones: string;
  estado: string;
  desEstado: string;
}

export interface IPostulantes {
  correlativo: number;
  nombre: string;
  nroDocumento: string;
  idPerfil: number;
  desPerfil: string;
  desGrupo: string;
  nota: number;
  estado: string;
  desEstado: string;
  idCabecera: number;
  idExamen: number;
  idPostulante: number;
  estadoExamen: number;
  toView: boolean;
}

export interface Convocatoria {
  idConvocatoria: number;
  codigoConvocatoria: string;
  regimen: string;
}

export interface Regimen {
  idRegimen: number;
  desRegimen: string;
}

export interface ListaConocimiento {
  correlativo: number;
  codigoConvocatoria: string;
  regimen: string;
  cantidadPerfil: number;
  cantidadGrupos: number;
  fechaInicioExamen: string;
  idEvalucion: number;
  idEstado: number;
  idEntidad: number;
  estado: string;
  settings: any;
}

export interface RequestListaConocimiento {
  convocatoriaId: number;
  idregimen: number;
  fecha: string;
  estadoExamen: number;
  entidadId: number;
}

export class ResponseResumenConocimiento {
  cabeceraPregunta: string;
  preguntaTexto: string;
  mensajePregunta: string;
  alternativas: string[];
}

export class ResponseRespuestaResumen {
  pregunta: string;
  estado: string;
}

export interface CboPerfil {
  idPerfil: number;
  convocatoriaId: number;
  idEntidad: number;
  evaluacionId: number;
  desPerfil: string;
}

export interface CboGrupo {
  idModalidad: number;
  desGrupo: string;
  idExamen: number;
  idProgramacion: number;
}

export interface CboExamen {
  idExamen: number;
  desExamen: string;
}

export interface CboNombre {
  postulanteId: number;
  nombreApellido: string;
}

export interface CboOtrosNombre {
  idPostulante: number;
  nombreCompleto: string;
}

export class NotaIndividual {
  perfilId: number;
  situacionId: number;
  puntaje: number;
  codigo: string;
  observacion: string;
  programacionId: number;
  desGrupo: string;
  postulanteId: number;
  desPostulante: string;

  flagSubirArchivo: boolean;
  documentoBase64: string;
  nombreArchivo: string;
}

export class OtraNotaIndividual {
  postulanteId: number;
  tipoEvaluacion: number;
  flagApto: number;
  fechaEvaluacion: string;
  perfilId: number;
  observacion: string;
  documentoBase64: string;
  nombreArchivo: string;
  flagSubirArchivo: boolean;
}

export class NotaMasiva {
  programacionId: number;
  documentoBase64: string;
}

export interface ResumenRespuestas {
  preguntasCorrectas: string[];
  preguntasIncorrectas: string[];
  preguntasAbiertas: string[];
  ptsTotPregCerrada: number;
  ptsObtPregCerrada: number;
  ptsTotPregAbierta: number;
  ptsObtPregAbierta: number;
  puntajeTotal: number;
}

export interface RespDatoConvocatoria {
  idConvocatoria: number;
  codigoConvocatoria: string;
  regimen: string;
  contadorPostulantes: number;
}
