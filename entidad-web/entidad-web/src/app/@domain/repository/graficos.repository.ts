import { Observable } from 'rxjs';

export abstract class GraficosRepository {
  abstract cabecera(
    rol: string,
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;
  abstract graficoBarrasPuestos(
    rol: string,
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;
  abstract graficoDonutPuestos(
    rol: string,
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoDonutEstadoPostulante(
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoBarrasPerfiles(
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoDonutEstadoBase(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoBarraEstadoConvocatoria(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoDonutGestores(
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract EstadoBasesByGestor(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract downloadExcelGestor(params: any): Observable<any>;
  abstract downloadExcelCoordinador(params: any): Observable<any>;
  abstract downloadExcelAdmin(params: any): Observable<any>;
  abstract downloadExcelServir(params: any): Observable<any>;

  abstract cabeceraServir(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoLineEntidadesServir(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoRegimenServir(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoBarraPromedioConvocatoria(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoUsuariosByEntidad(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any>;

  abstract graficoUsuariosByConvDifusion(
    condicion: string,
    fechaIni: string,
    fechaFin: string,
    entidadId: number
  ): Observable<any>;

  abstract graficoUsuariosByConvEstados(
    condicion: string,
    fechaIni: string,
    fechaFin: string,
    entidadId: number
  ): Observable<any>;
}
