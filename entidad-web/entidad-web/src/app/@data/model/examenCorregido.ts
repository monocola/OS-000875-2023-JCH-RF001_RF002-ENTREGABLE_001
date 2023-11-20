export interface ExamenCorregidoResumen {
  puntajeTotal: number;
  fechaEvaluacion: string;
  situacionExamen: string;
  tiempoExamen: string;
  cantidadpreguntas: string;
  lstPreguntas: RespPreguntaCorregida[];
}
export interface RespPreguntaCorregida {
  preguntaId: number;
  originalId: number;
  descripcion: string;
  tipoPregunta: number;
  estadoRespuesta: number;
  correcta: string;
  orden: number;
  TextoPregunta: number;
  alternativa: AlternativaResumen[];
}
export interface AlternativaResumen {
  alternativaId: number;
  originalId: number;
  descripcion: string;
  tipoPregunta: number;
  estadoRespuesta: number;
  seleccion: boolean;
}


export interface CapturaImagenResumen {
  fotografiaId: number;
  postulanteId: number;
  examenId: number;
  urlImagen: string;
}
