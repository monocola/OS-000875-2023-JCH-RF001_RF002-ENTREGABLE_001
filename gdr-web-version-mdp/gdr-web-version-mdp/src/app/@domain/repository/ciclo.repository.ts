import { Observable } from 'rxjs';

export abstract class CicloRepository {
  abstract getListCiclo(entidadId: number): Observable<any[]>;
  abstract getPuesto(entidadId: number , puesto: string): Observable<any[]>;
  abstract registraCiclo(
    entidadId: number,
/*     puestoTitularId: number,
    puestoJefeId: number,
    puestoGestor: number, */
    anio: number,
    fechaIni: Date,
    fechaFin: Date,
/*     descripcion: string,
    estadoCicloId: number, */
    estadoCicloDesc: string,
  ): Observable<any>;
  abstract getCiclosFilter(): Observable<any>;
  abstract getCiclosFilterFormalizados(): Observable<any>;

}
