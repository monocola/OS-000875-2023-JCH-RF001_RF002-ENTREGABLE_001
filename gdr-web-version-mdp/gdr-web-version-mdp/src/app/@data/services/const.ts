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
  public static TPO_AGENDAMIENTO_ID: number;

  public static API_NOTIFICACION: string;
  public static NAME_APP_GDR: string;
  public static NAME_APP_GME: string;
  public static URL_GOOGLE_MEET: string;
  public static TITLE_REUNION: string;
  public static CORTAR_NUM_CARACTERES_PARTICIPANTES: number;

  public static ESTADO_INACTIVO_SERVIDORES_CIVILES: number;


  constructor() {}

  public loadCommonConfig() {
    Const.API_FILE_SERVER = environment.public_base_url_file_server;
    Const.API_SEGURIDAD = environment.public_base_url_seguridad;
    Const.API_PERSONA = environment.public_base_url_persona;
    Const.API_MAESTRA = environment.public_base_url_maestra;
    Const.API_ENTIDAD = environment.public_base_url_entidad;
    Const.API_PLANIFICACION = environment.public_base_url_planificacion;
    Const.API_NOTIFICACION = environment.public_base_url_notificacion;
    Const.GESTOR_ID = environment.gestor_id;
    Const.NAME_APP_GDR = 'gdr';
    Const.NAME_APP_GME = 'gme';
    Const.URL_GOOGLE_MEET = environment.url_google_mett;
    Const.TITLE_REUNION = 'REUNIÓN USUARIO ORH';
    Const.CORTAR_NUM_CARACTERES_PARTICIPANTES = 30;
    Const.TPO_AGENDAMIENTO_ID = 412;
    Const.ESTADO_INACTIVO_SERVIDORES_CIVILES = 4;

  }

  public loadEntidadConfig() {
    Const.USERNAME_SEGURIDAD = environment.client_id;
    Const.PASSWORD_SEGURIDAD = environment.client_secret;
    Const.APPLICATION_ID = environment.aplicacion_id;
  }

  public static MOCK_GDR_MENU = {
    "trace": {
      "traceId": null
    },
    "status": {
      "success": true,
      "error": {
        "code": null,
        "httpCode": null,
        "messages": []
      }
    },
    "payload": {
      "treeMenusByAplicacion": [
        {
          "menuId": 141,
          "aplicacionId": 6,
          "menuPadre": null,
          "descripcion": "Tablero informativo",
          "icono": "dashboard",
          "url": "/home",
          "orden": 1,
          "estadoRegistro": 1,
          "subMenu": []
        },
        {
          "menuId": 114,
          "aplicacionId": 6,
          "menuPadre": null,
          "descripcion": "Maestras",
          "icono": "security",
          "url": "/maestras",
          "orden": 2,
          "estadoRegistro": 1,
          "subMenu": [
            {
              "menuId": 134,
              "aplicacionId": 6,
              "menuPadre": 114,
              "descripcion": "Servidores Civiles",
              "icono": "bandeja",
              "url": "/servidores",
              "orden": 2,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 126,
              "aplicacionId": 6,
              "menuPadre": 114,
              "descripcion": "Registrar organigrama",
              "icono": "bandeja",
              "url": "/organigrama",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            }
          ]
        },
        {
          "menuId": 102,
          "aplicacionId": 6,
          "menuPadre": null,
          "descripcion": "Planificación",
          "icono": "work_outline",
          "url": "/planificacion",
          "orden": 3,
          "estadoRegistro": 1,
          "subMenu": [
            {
              "menuId": 140,
              "aplicacionId": 6,
              "menuPadre": 102,
              "descripcion": "Registrar Ciclo",
              "icono": "bandeja",
              "url": "/ciclos",
              "orden": 2,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 145,
              "aplicacionId": 6,
              "menuPadre": 102,
              "descripcion": "Cronograma",
              "icono": "bandeja",
              "url": "/cronograma",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 115,
              "aplicacionId": 6,
              "menuPadre": 102,
              "descripcion": "Evaluados",
              "icono": "bandeja",
              "url": "/evaluados",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 116,
              "aplicacionId": 6,
              "menuPadre": 102,
              "descripcion": "Resumen",
              "icono": "bandeja",
              "url": "/resumen",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 103,
              "aplicacionId": 6,
              "menuPadre": 102,
              "descripcion": "Reuniones",
              "icono": "bandeja",
              "url": "/reuniones",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 144,
              "aplicacionId": 6,
              "menuPadre": 102,
              "descripcion": "Participantes",
              "icono": "bandeja",
              "url": "/participantes",
              "orden": 5,
              "estadoRegistro": 1,
              "subMenu": []
            }
          ]
        },
        {
          "menuId": 112,
          "aplicacionId": 6,
          "menuPadre": null,
          "descripcion": "Seguimiento",
          "icono": "portrait",
          "url": "/seguimiento",
          "orden": 4,
          "estadoRegistro": 1,
          "subMenu": [
            {
              "menuId": 118,
              "aplicacionId": 6,
              "menuPadre": 112,
              "descripcion": "Evaluados",
              "icono": "bandeja",
              "url": "/evaluados",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 119,
              "aplicacionId": 6,
              "menuPadre": 112,
              "descripcion": "Resumen",
              "icono": "bandeja",
              "url": "/resumen",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 117,
              "aplicacionId": 6,
              "menuPadre": 112,
              "descripcion": "Reuniones",
              "icono": "bandeja",
              "url": "/reuniones",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            }
          ]
        },
        {
          "menuId": 113,
          "aplicacionId": 6,
          "menuPadre": null,
          "descripcion": "Evaluacion",
          "icono": "list_alt",
          "url": "/evaluacion",
          "orden": 5,
          "estadoRegistro": 1,
          "subMenu": [
            {
              "menuId": 121,
              "aplicacionId": 6,
              "menuPadre": 113,
              "descripcion": "Evaluados",
              "icono": "bandeja",
              "url": "/evaluados",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 122,
              "aplicacionId": 6,
              "menuPadre": 113,
              "descripcion": "Resumen",
              "icono": "bandeja",
              "url": "/resumen",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            },
            {
              "menuId": 120,
              "aplicacionId": 6,
              "menuPadre": 113,
              "descripcion": "Reuniones",
              "icono": "bandeja",
              "url": "/reuniones",
              "orden": 3,
              "estadoRegistro": 1,
              "subMenu": []
            }
          ]
        },
        {
          "menuId": 111,
          "aplicacionId": 6,
          "menuPadre": null,
          "descripcion": "Notificacion",
          "icono": "announcement",
          "url": "/notificacion",
          "orden": 6,
          "estadoRegistro": 1,
          "subMenu": []
        }
      ]
    }
  };
}
