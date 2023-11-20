import { HttpClient, HttpParams, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { ImplementacionRepository } from 'src/app/@domain/repository/implementacion.repository';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { TypeDocument } from '../model/typeDocument';
import { ApiEntidad, ApiPlaniEntidad } from '../model/entidad';
import { Observable } from 'rxjs';
import { EntidadGestor } from '../model/entidadGestor';
import { ItemMenu } from '../model/item-menu';
import { SidenavService } from './sidenav.service';
import {UsuarioGdr} from "../model/usuarioGdr";
import { PermissionRepository } from '../../@domain/repository/permission.repository';
import { PermissionLevel } from './permission.service';
import { ComboItem } from '../model/comboItem';
import { GenericResponse } from '../model/generic/generic.response';

@Injectable({
  providedIn: 'root',
})
export class ImplementacionService implements ImplementacionRepository {
  apiEntidad: ApiEntidad[] = [];
  entidadesGestores: EntidadGestor[] = [];


  constructor(
    private http: HttpClient,
    private sidenavService: SidenavService,
    private permissionRepository: PermissionRepository,
  ) {}


  getParametros(value: string): Observable<any[]> {
    throw new Error('Method not implemented.');
  }

  getSector(): Observable<TypeDocument[]> {
    const parametro = 'ENT_SECTOR';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getListEntidades(): Observable<ApiEntidad[]> {
    const url = `${Const.API_ENTIDAD}v1/entidad`;
    return this.http.get(url).pipe(
      map((response: any) => {
        this.apiEntidad = response.payload.entidad;
        this.apiEntidad.forEach( item => {
          item.siglaDescripcion = item.sigla + ' - ' + item.descripcionEntidad;
        });
        // return response.payload.entidad;
        return this.apiEntidad;
      })
    );
  }

  getListImplementacion(ciclo, nivelGob?, sector?, tipEntidad?: number, entidad?: string): Observable<ApiPlaniEntidad[]> {
    let url = `${Const.API_PLANIFICACION}v1/entidad?anio=${ciclo}`;
    if ( nivelGob ) { url = url + `&nivelGobiernoId=${nivelGob}`; }
    if ( sector ) { url = url + `&sectorId=${sector}`; }
    if ( tipEntidad ) { url = url + `&tipoEntidadPubId=${tipEntidad}`; }
    if ( entidad ) { url = url + `&descripcion=${entidad}`; }
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items.map(item => {
          item.contGestor = item.conteoGestorGDR.toString();
          item.colorEstado = '<span class="' + item.estado +
            '"><span class="dot"></span>&nbsp&nbsp' + item.estado + '</span>';
          return item;
        });
      })
    );
  }

  getObtenerPDFs(uuId: String): any {
    let url = `${Const.API_PLANIFICACION}v1/asignacion/obtenerGdrPdf?uiid=${uuId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  getListUsuariosGdr(entidadId): Observable<UsuarioGdr[]> {
    let url = `${Const.API_PLANIFICACION}v1/gestores/entidad/${entidadId}`;
      return this.http.get(url).pipe(
        map((response: any) => {
            let listaUsuario: UsuarioGdr[] = response.payload.items;
            return listaUsuario.map(item => {
              item.disabledNotification = item.estadoRegistro !== '1';
              if (!item.disabledNotification) {
                item.estado = "Activo";
              } else {
                item.estado = "Inactivo";
              }
              item.colorEstado = `<span class="${item.estado.toLowerCase()}"><span class="dot"></span>&nbsp&nbsp${item.estado.toUpperCase()}</span>`;
              return item;
            });

        })
      );
  }

  getListadoEntidadRector(rectorId, flagSoloAsociadas?, nivelGob?, sector?, tipoEntidadPubId?: number, entidad?: string): Observable<EntidadGestor[]> {
    let url = `${Const.API_PLANIFICACION}v1/entidades/activas/${rectorId}?flagSoloAsociadas=${flagSoloAsociadas}`;
    if ( sector ) { url = url + `&sectorId=${sector}`; }
    if ( nivelGob ) { url = url + `&nivelGobiernoId=${nivelGob}`; }
    if ( tipoEntidadPubId ) { url = url + `&tipoEntidadPubId=${tipoEntidadPubId}`; }
    if ( entidad ) { url = url + `&descripcion=${entidad}`; }
    let contador = 0;
    return this.http.get(url).pipe(
      map((response: any) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            this.entidadesGestores = response.payload.items;
            this.entidadesGestores.forEach(item => {
              item.nro = (contador + 1).toString();
              contador++;
            });
            return this.entidadesGestores;
          }
        }
      )
    );
  }

  getOptionsMenuTemp(): Observable<boolean> {
    const url = `${Const.API_SEGURIDAD}v1/roles/${Const.GESTOR_ID}/aplicacion/${Const.APPLICATION_ID}/subMenus`;
    return this.http.get(url).pipe(
    // return of(Const.MOCK_GDR_MENU).pipe(
      map((response: any) => {
        const menu: ItemMenu[] = response.payload.treeMenusByAplicacion;
        menu.map((item) => {
          item.opened = false;
          return item;
        });
        sessionStorage.setItem('originalMenu', JSON.stringify(this.sidenavService.menu));
        this.permissionRepository.setPermission(PermissionLevel.READ);
        console.log("menu:",menu)
        this.sidenavService.setMenu(menu);
        return true;
      })
    );
  }

  restoreMenu(): void {
    this.sidenavService.setMenu(JSON.parse(sessionStorage.getItem('originalMenu')));
    sessionStorage.removeItem('originalMenu');
    let entidadOriginal = sessionStorage.getItem('originalEntidad');
 
   if(entidadOriginal!==null) {
      sessionStorage.removeItem('entidad');
      sessionStorage.removeItem('originalEntidad');
      sessionStorage.setItem('entidad', entidadOriginal);
    }

    this.permissionRepository.setPermission(PermissionLevel.READANDWRITE);
  }

  validateTempMenu(): boolean {
    return sessionStorage.getItem('originalMenu') != null;
  }

  getDetailGestorGDR(gestorEntidadID: number): Observable<UsuarioGdr> {
    let url = `${Const.API_PLANIFICACION}v1/gestores/${gestorEntidadID}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        let result = response.payload;
        result.estadovalue = response.payload.estadoRegistro === "1";
        return result;
      })
    );
  }

  getTypeDocuments(): Observable<ComboItem[]> {
    const parametro = 'SOL_TIPO_DOCUMENTO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(map((response: any) => {
      return response.payload.items.map(item => {
        return { value: item.valorNumero, description: item.valorTexto };
      });
    }));
  }
 
  getServidoresCiviles(entidadId: number, tipoDocumentoId: number, nroDocumento: number): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/servidoresCiviles/puestosUO?entidadId=${entidadId}&tipoDocumentoId=${tipoDocumentoId}&nroDocumento=${nroDocumento}`;
    return this.http.get(url).pipe(map((response: any) => {
      if (response.status.success) {
        return response.payload;
      } else {
        throw new Error(response.status.error.messages[0]).message;
      }
    }));
  } 

  updateGestor(data: any, archivo: any): Observable<any> {
    const formData: FormData = new FormData();
    formData.append("archivo", archivo);
    formData.append("nombreArchivo", data.nombreArchivo);
    formData.append("gestorEntidadId", data.gestorEntidadId);
    formData.append("estadoRegistro", data.estado ? "1" : "0");
    formData.append("flagAsignacion", "1");
    formData.append("uuidId", data.uuidId);

    const url = `${Const.API_PLANIFICACION}v1/gestores`;
    const req = new HttpRequest(
      'PUT',
      url,
      formData,
      {
        reportProgress: true,
        responseType: 'json',
      }
    );
    return this.http.request(req);
  }

  newGestor(data: any): Observable<GenericResponse<void>> {

    const formData: FormData = new FormData();
    formData.append("archivo", data.file);

    let params = new HttpParams();
    params = params.append('nombreArchivo', data.file != null ? data.file.name : null);
    params = params.append('flagAsignacion', data.file !== null ? "1" : "0");
    delete data.file;
    data.rolId = Const.GESTOR_ID;
    Object.keys(data).forEach(key => {
      params = params.append(key, data[key]);
    });


    const url = `${Const.API_PLANIFICACION}v1/gestores`;
    return this.http.post<GenericResponse<void>>(url, formData, {params: params}).pipe(map(response => {
      if (response.status.success) {
        return response;
      } else {
        throw new Error(response.status.error.messages[0]).message;
      }
    }));
  }
}


