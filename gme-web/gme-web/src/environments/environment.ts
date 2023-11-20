/**
 * @license
 * Copyright Akveo. All Rights Reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 */
// The file contents for the current environment will overwrite these during build.
// The build system defaults to the dev environment which uses `environment.ts`, but if you do
// `ng build --env=prod` then `environment.prod.ts` will be used instead.
// The list of which env maps to which file can be found in `.angular-cli.json`.

export const environment = {
  production: false,

  public_base_url_maestra: "https://devapi1.servir.gob.pe/maestra-mdp/api/public/",
  // public_base_url_maestra: "http://localhost:8081/maestra-mdp/api/public/",

  public_base_url_persona: "https://devapi1.servir.gob.pe/persona-mdp/api/public/",
  public_base_url_seguridad: "https://devapi1.servir.gob.pe/seguridad-mdp/api/public/",
  // public_base_url_seguridad: "http://localhost:8081/seguridad-mdp/api/public/",

   public_base_url_entidad: "https://devapi1.servir.gob.pe/entidad-mdp/api/public/",
  //public_base_url_entidad: "http://localhost:8082/entidad-mdp/api/public/",
  public_base_url_planificacion: "https://devapi1.servir.gob.pe/planificacion-mdp/api/public/",
  // public_base_url_planificacion: "http://localhost:8083/planificacion-mdp/api/public/",
  //public_base_url_file_server: "http://172.16.19.41/file/",
  public_base_url_file_server: "https://172.16.19.249/file/",
  public_base_url_notificacion: 'https://devapi1.servir.gob.pe/mensajeria-mdp/api/public/',
  //public_base_url_notificacion: 'http://localhost:8084/mensajeria-mdp/api/public/',
  client_id: "98298d69-c9f5-4987-a395-fe8ed9ec973a",
  client_secret: "7cf28133-24b7-4ee9-b9a4-988d158e0ad6",
  aplicacion_id: 4,
  gestor_id: 16,
};
