export class SaveBonificacion {
  bonificacionDTO: Bonificacion;
  constructor(bonificacion: Bonificacion) {
    this.bonificacionDTO = bonificacion;
  }
}
export class DetailBonificacion {
  bonificacion: Bonificacion;
  constructor(bonificacion: Bonificacion) {
    this.bonificacion = bonificacion;
  }
}

export class ConfirmarBonificacion {
  bonificacionPostulanteId: number;
  tipoBonificacion: number;
  tipo: number;
  comentario: string;
  flagAplica: number;
}

export interface Bonificacion {
  bonificacionId: any;
  tipoInfoId: any;
  tipoBonificacion: any;
  titulo: any;
  contenido: any;
  estadoRegistro: any;
  bonificacionDetalleDTOList:
    | [
        {
          bonificacionDetalleId: any;
          bonificacionId: any;
          descripcion: any;
          nivelId: any;
          aplicaId: any;
          porcentajeBono: any;
          nombreNivel: any;
          nombreAplica: any;
          estadoRegistro: any;
        }
      ]
    | any;
}
