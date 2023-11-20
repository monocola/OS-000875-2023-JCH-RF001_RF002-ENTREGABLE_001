import { Observable } from "rxjs";
import { GestoresOrh } from "src/app/@data/model/gestores-orh";

export abstract class GestoresOrhRepository {

  abstract getList(entidadId: number): Observable<GestoresOrh[]>;
  abstract deleteGestorOrh(gestorId: number): Observable<GestoresOrh>;

  abstract setRegistrarGestor(body: any): Observable<any>;
  abstract getGestorId(gestorId: number): Observable<any>;
  abstract saveGestorORH(body: any): Observable<any>;

}
