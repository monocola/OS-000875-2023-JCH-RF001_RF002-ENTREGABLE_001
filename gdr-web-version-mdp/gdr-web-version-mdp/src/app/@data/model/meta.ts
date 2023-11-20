export interface MetaParticipante {
  numeracion: number;
  metaId: number;
  personaId: number;
  detaUoId?: any;
  fuente: string;
  prioridad: number;
  indicador: string;
  peso: number;
  datoPeso: string;
  valorMeta: string;
  datoValorMeta: string;
  estadoRegistro: string;
  estadoMeta: string;
  usuarioCreacion: string;
  fechaCreacion: string;
  tipoIndicadorProducto: string;
  cicloId: number;
  tipoMeta: string;
  validadoEvaluador: string;
  evidencia: EvidenciaParticipante[];
  colorEstadoMeta: string;
}

export interface EvidenciaParticipante {
  numeracion: number;
  evidenciaId: number;
  metaId: number;
  evidencia: string;
  plazo: string;
  estadoRegistro: string;
}

export interface PayloadMetas {
  listaMeta: MetaParticipante[];
  evidencia: EvidenciaParticipante[];
  pesoToTal: number;
  validadoEvaluador: string;
  uuid: string;
  nombreArchivo: string;
}
