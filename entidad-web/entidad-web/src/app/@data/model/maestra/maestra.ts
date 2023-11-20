export interface MaestraCebecera {
  estadoRegistro: string;
  usuarioCreacion: string;
  fechaCreacion: string;
  usuarioModificacion?: any;
  fechaModificacion?: any;
  maeCabeceraId: number;
  codigoCabecera: string;
  descripcion: string;
  descripcionCorta?: any;
  soloServir: string;
  listMaestraDetalles?: any;
  listaMaestraDetalleEntidads?: any;
}

export interface MaestraDetalle {
  estadoRegistro: string;
  usuarioCreacion?: any;
  fechaCreacion?: any;
  usuarioModificacion?: any;
  fechaModificacion?: any;
  maeDetalleId: number;
  maeCabeceraId: number;
  descripcion: string;
  descripcionCorta: string;
  sigla: string;
  referencia: string;
  orden: number;
  codProg: string;
  estado: string;
  configuracionId?: any;
  estadoConfiguracion?: any;
  listaDetalleModalidad?: any;
  listaTipo?: any;
  listaEvaluacion?: any;
}

export interface ResponseMaestra {
  maestraCebecera: MaestraCebecera;
  maestraDetalles: MaestraDetalle[];
}
