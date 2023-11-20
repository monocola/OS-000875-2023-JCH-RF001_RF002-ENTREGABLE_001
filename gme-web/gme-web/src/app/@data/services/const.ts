import { Injectable } from '@angular/core';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root',
})
export class Const {
  public static API_FILE_SERVER: string;
  public static API_SEGURIDAD: string;
  public static API_PERSONA: string;
  public static API_MAESTRA: string;
  public static API_ENTIDAD: string;
  public static API_CONVOCATORIA: string;
  public static API_PLANIFICACION: string;
  public static USERNAME_SEGURIDAD: string;
  public static PASSWORD_SEGURIDAD: string;
  public static APPLICATION_ID: number;
  public static GESTOR_ID: number;
  public static API_NOTIFICACION: string;

  public loadCommonConfig() {
    Const.API_FILE_SERVER = environment.public_base_url_file_server;
    Const.API_SEGURIDAD = environment.public_base_url_seguridad;
    Const.API_PERSONA = environment.public_base_url_persona;
    Const.API_MAESTRA = environment.public_base_url_maestra;
    Const.API_ENTIDAD = environment.public_base_url_entidad;
    Const.API_PLANIFICACION = environment.public_base_url_planificacion;
    Const.API_NOTIFICACION = environment.public_base_url_notificacion;
    Const.GESTOR_ID = environment.gestor_id;
  }

  public loadEntidadConfig() {
    Const.USERNAME_SEGURIDAD = environment.client_id;
    Const.PASSWORD_SEGURIDAD = environment.client_secret;
    Const.APPLICATION_ID = environment.aplicacion_id;
  }
}
