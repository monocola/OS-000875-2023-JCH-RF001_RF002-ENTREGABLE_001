import { Observable } from 'rxjs';
import { NotaIndividual } from 'src/app/@data/model/lista-evaluaciones/entity';
import { NotaMasiva } from '../../@data/model/lista-evaluaciones/entity';

export abstract class ListaGestionNotasRepository {
  abstract getListaEvaluacionesPostulante(
    filtro: any,
    convocatoriaId: string
  ): Observable<any>;

  abstract getComboEvaluaciones(convocatoriaId: string): Observable<any>;

  abstract getComboPostulantes(programacionId: number): Observable<any>;

  abstract getPDFcv(cabeceraId: number): Observable<any>;

  abstract guardarNotaExamenIndividual(
    individual: NotaIndividual
  ): Observable<any>;

  abstract guardarNotaExamenMasivo(request: NotaMasiva): Observable<any>;

  abstract downloadTemplate(programacionId: number): Observable<any>;

  abstract getComboGrupos(
    convocatoriaId: string,
    perfilId: number
  ): Observable<any>;

  abstract getComboExamen(
    convocatoriaId: string,
    perfilId: number
  ): Observable<any>;

  abstract getComboEvaluacionesPostulante(convocatoriaId: number): Observable<any>;

  abstract getDataTableAll(convocatoriaId: number): Observable<any>;

  abstract saveNotaExamenMasivo(programacionId: number, multipartData: FormData): Observable<any>;
}
