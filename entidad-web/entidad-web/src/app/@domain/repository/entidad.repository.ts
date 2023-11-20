import { Observable } from 'rxjs';
import { Entidad } from '../../@data/model/entidad';

export abstract class EntidadRepository {
  abstract getRegistro(id: number): Observable<Entidad>;
  abstract saveRegistro(
    registroEntidad: Entidad,
    imgData64: string,
    imgFile: File,
    entidad: Entidad,
    flagPhoto: number,
    imgData64P: string,
    imgFileP: File,
    flagPhotoP: number
  ): Observable<boolean>;
  abstract getEntidades(): Observable<Entidad []>;
  abstract getUsuariosEntidad(entidadId: string): Observable<any>;
}
