import { Observable } from 'rxjs';

export abstract class ProcesoEvaluacionRepository {
  abstract comboEstadoEvaCurricular(): Observable<any>;

  abstract comboEstadoReqMinimos(): Observable<any>;

  abstract listarConfiguracionPerfil(
    nombrePerfil: string,
    regimenId: number,
    rolId: number,
    estadoRmId: number,
    estadoEcId: number
  ): Observable<any>;
}
