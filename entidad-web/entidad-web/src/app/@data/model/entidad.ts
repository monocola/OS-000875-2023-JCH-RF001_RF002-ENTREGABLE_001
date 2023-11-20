import { Injectable } from '@angular/core';
import { Const } from '../../@data/services/const';
import { Adapter } from './adapter';

export class Entidad {

  constructor(
    public anexo: string,
    public correo: string,
    public correoId: string,
    public departamento: string,
    public departamentoId: number,
    public descripcionEntidad: string,
    public direccion: string,
    public direccionFiscal: string,
    public direccionId: number,
    public distrito: string,
    public distritoId: number,
    public entidadId: number,
    public logo: string,
    public nivelGobierno: string,
    public nivelGobiernoId: number,
    public nombreComercial: string,
    public numeroDocumento: string,
    public personaId: number,
    public provincia: string,
    public provinciaId: number,
    public razonSocial: string,
    public sector: string,
    public sectorId: number,
    public sigla: null,
    public telefono: null,
    public telefonoId: null,
    public tipoDocumento: number,
    public urlWeb: null,
    public urlPortada: string
  ) { }

}

@Injectable({
  providedIn: "root",
})
export class EntidadAdapter implements Adapter<Entidad> {

  adapt(response: Entidad): Entidad {
    return new Entidad(
      response.anexo,
      response.correo,
      response.correoId,
      response.departamento,
      response.departamentoId,
      response.descripcionEntidad,
      response.direccion,
      response.direccionFiscal,
      response.direccionId,
      response.distrito,
      response.distritoId,
      response.entidadId,
      response.logo ? Const.API_FILE_SERVER + response.logo : null,
      response.nivelGobierno,
      response.nivelGobiernoId,
      response.nombreComercial,
      response.numeroDocumento,
      response.personaId,
      response.provincia,
      response.provinciaId,
      response.razonSocial,
      response.sector,
      response.sectorId,
      response.sigla,
      response.telefono,
      response.telefonoId,
      response.tipoDocumento,
      response.urlWeb,
      response.urlPortada ? Const.API_FILE_SERVER + response.urlPortada : null
    );
  }


}
