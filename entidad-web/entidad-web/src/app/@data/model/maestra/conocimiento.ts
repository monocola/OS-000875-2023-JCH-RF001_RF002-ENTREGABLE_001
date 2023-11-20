export interface LstMaestraConocimiento {
  maeConocimientoId: number;
  tipoConocimientoId: number;
  categoriaConocimientoId: number;
  descripcion: string;
  entidadId?: number;
  horas: number;
  codigoTipoConocimiento: string;
  descripcionTipo: string;
  descripcionCategoria: string;
  estado: string;
  nivelDominioId: number;
}

export class MaestraConocimiento {
  lstMaestraConocimiento: LstMaestraConocimiento[] = [];
}

export class ConocimientoAgrupado {
  descripcionCategoria: string;
  hijos: string[] = [];
}

export class ConocimientoDataTratada {
  conocimientoAgrupado: ConocimientoAgrupado[] = [];
  listaoriginal: LstMaestraConocimiento[] = [];
}
