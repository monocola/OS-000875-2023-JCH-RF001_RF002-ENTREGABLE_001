import { Observable } from 'rxjs';

export abstract class DesestimientoRepository {
  abstract DesestimarContrato(
    contratoId:number,
    categoriaDesestimientoId: number,
    motivoDesestimiento: string,
    estadoId: number,
    pdfBase64: string,
    tipoTrabajo: string,
    postulante:any,
  ): Observable<any>;
  abstract SuscribirContrato(
    contratoId: number,
    estadoId: number,
    pdfBase64: string,
    fechaSuscripcion: string,
  ): Observable<any>;
  abstract DescargarContrato(
    contratoId: number,
  ): Observable<any>;
}
