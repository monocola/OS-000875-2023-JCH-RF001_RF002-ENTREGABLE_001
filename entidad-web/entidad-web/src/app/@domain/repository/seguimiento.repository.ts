import { Observable } from 'rxjs';
import { ComboitemModel } from '../../@data/model/generic/comboitem.model';

export abstract class SeguimientoRepository {
  abstract getFiltroMaestra(keymae: string): Observable<ComboitemModel[]>;
  abstract getData(filtros: any, page: number, size: number): Observable<any>;
  abstract getDataSeguimiento(
    filtros: any,
    page: number,
    size: number
  ): Observable<any>;
  abstract getComunicados(
    convocatoriaId: number,
    perfilId: number,
    etapaId: number,
    fechaIni: string,
    fechaFin: string,
    tipoComunicadoId: number,
    estadoId: number,
    page: number,
    size: number
  ): Observable<any>;
  abstract saveComunicado(
    convocatoriaId: any,
    perfilId: any,
    etapaId: any,
    tipoComunicadoId: any,
    file: any,
    estadoId: any
  ): Observable<any>;
  abstract inactivarComunicado(comunicadoId: number): Observable<any>;
  abstract getComunicado(comunicadoId: number): Observable<any>;
  abstract updateComunicado(
    comunicadoId: any,
    convocatoriaId: any,
    perfilId: any,
    etapaId: any,
    estadoId: any,
    tipoComunicadoId: any,
    gestorId: any,
    coordinadorId: any,
    nombreGestor: any,
    nombreCoordinador: any,
    file: any
  ): Observable<any>;
  abstract updateEstadoComunicado(
    estadoId: number,
    observacion: string,
    gestorId: number,
    coordinadorId: number,
    nombreGestor: string,
    nombreCoordinador: string,
    comunicadoId: number,
    estadoConvocatoriaId: number
  ): Observable<any>;
  abstract CrearContratoOrConvenio(
    tipoTrabajo: number,
    params: any,
    tipoContrato: number
  ): Observable<any>;
  abstract getContratosOrConvenios(
    tipo: number,
    regimen: number,
    tipoPractica: number,
    perfil: number,
    fechaInicio: string,
    fechaFin: string,
    estado: number,
    page: number,
    size: number
  ): Observable<any>;
}
