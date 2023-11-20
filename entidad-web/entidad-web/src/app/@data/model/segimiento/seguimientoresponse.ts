export interface Seguimiento {
  convocatoriaId: number;
  codigoConvocatoria?: any;
  codConvAndCodRegimen: string;
  baseId: number;
  etapa: string;
  codProgEtapa: string;
  estadoConvocatoria: string;
  url?: any;
  correlativo?: any;
  anio?: any;
  vacantes?: any;
  postulantesAndCalifican: string;
  postulantes?: any;
  califican?: any;
  gestor: string;
  coordinador?: any;
  fechaPublicacion: string;
}

export interface SegimientoResponse {
  count: number;
  items: Seguimiento[];
}
