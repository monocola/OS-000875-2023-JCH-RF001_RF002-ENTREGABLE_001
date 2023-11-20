import { Observable } from 'rxjs';
import { OtraNotaIndividual } from 'src/app/@data/model/lista-evaluaciones/entity';

export abstract class ListaGestionOtrasNotasRepository {
  abstract getComboPerfil(convocatoriaId: string): Observable<any>;

  abstract getComboPostulantes(
    convocatoriaId: string,
    perfilId: number
  ): Observable<any>;

  abstract guardarOtraNotaIndividual(
    individual: OtraNotaIndividual,
    convocatoriaId: number
  ): Observable<any>;

  abstract guardarCondicionMasivo(request: any): Observable<any>;

  abstract downloadTemplate(): Observable<any>;

  abstract listaEstadoEvaluacion(flag: any): Observable<any>;

  abstract guardarCondicionMasivoMtp(request: any, multipartData: FormData): Observable<any>;

  abstract guardarOtraNotaIndividualMtp(multipartData: FormData, perfilId: number, convocatoriaId: number): Observable<any>;

}
