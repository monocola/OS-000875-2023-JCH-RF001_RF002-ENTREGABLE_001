import { Observable } from 'rxjs';
import { Base } from 'src/app/@data/model/base';
import { UpdateEstado } from '../../@data/model/bases/update.estado';

export abstract class BasesRepository {
  abstract getBases(body: any): Observable<Base[]>;
  abstract deleteBase(baseId: number): Observable<boolean>;
  abstract getPDFInforme(informeDetalleId: number): Observable<any>;
  abstract getPDF(
    idInforme: number,
    idTipoInformacion: number,
    baseId: number
  ): Observable<any>;

  abstract getDataStep1(baseId: number): Observable<any>;

  abstract getPDFPerfil(idPerfil: number): Observable<any>;
  abstract getDataStep2(baseId: number): Observable<any>;
  abstract getIndividualPerfilBase(basePerfilId: number): Observable<any>;
  abstract deleteIndividualPerfilBase(basePerfilId: number): Observable<any>;

  abstract getDataStep3(baseId: any): Observable<any>;
  abstract getDataStep4(baseId: any): Observable<any>;
  abstract getDataStep6(baseId: any): Observable<any>;

  abstract saveOrUpdateStep1(body: any, baseId: any): Observable<any>;
  abstract saveOrUpdateStep2(
    body: any,
    baseId: any,
    bodySede: any,
    basePerfilId?: any
  ): Observable<any>;
  abstract saveOrUpdateStep3(
    body: any,
    baseId: any,
    requisitoId: number
  ): Observable<any>;
  abstract saveOrUpdateStep4(
    baseId: number,
    observacion: string,
    params: any
  ): Observable<any>;
  abstract saveOrUpdateStep6(
    baseId: any,
    observaciones: string,
    informes: any
  ): Observable<any>;

  abstract getEtapas(params: any): Observable<any>;
  abstract getListaCronogramas(baseId: number): Observable<any>;
  abstract getListaCronogramasV2(baseId: number): Observable<any>;
  abstract guardarBaseCronograma(params: any): Observable<any>;
  abstract guardarBaseCronogramaV2(params: any): Observable<any>;
  abstract eliminarCronograma(id: number): Observable<any>;
  abstract editarCronograma(params: any, id: number): Observable<any>;
  abstract editarCronogramaV2(params: any): Observable<any>;
  abstract getCriteriosDeEvaluacion(): Observable<any>;
  abstract getListaDeEvaluaciones(baseId: number): Observable<any>;
  abstract getTiposDeInforme(params: any): Observable<any>;
  abstract getInformes(tipoInformeId: number): Observable<any>;
  abstract publicarBase(data: any): Observable<boolean>;

  abstract saveMovimientosObservaciones(
    oldEstado,
    newEstado,
    baseId: number,
    observaciones: any[]
  ): Observable<any>;
  abstract getMovimientosObs(baseId: number): Observable<any>;
  abstract obtenerPdf(idBase: string): Observable<any>;
  abstract obtenerPdfNew(perfilId: string): Observable<any>;
  abstract getListaCronogramasHistorico(baseId: number): Observable<any>;
}
