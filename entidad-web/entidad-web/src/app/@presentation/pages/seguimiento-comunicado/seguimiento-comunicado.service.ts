import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Const } from '../../../@data/services/const';

@Injectable({
  providedIn: 'root',
})
export class SeguimientoComunicadoService {
  form: FormGroup;
  formConvocatoria: FormGroup;

  constructor(private fb: FormBuilder) {}

  initializeValues() {
    this.initializeForm();
  }

  enviarConvocatoria(
    resp,
    baseId: number,
    etapaId: number,
    desEtapa: string,
    nomConv: string,
    editable: boolean
  ) {
    this.form.patchValue({
      baseId: baseId,
      estado: resp.estado,
      comunicadoId: resp.comunicadoId === undefined ? 0 : resp.comunicadoId,
      codProgEstado: resp.codProgEstado,
      etapaId: etapaId,
      desEtapa: desEtapa,
      nomConvocatoria: nomConv,
      editable: editable,
    });
  }

  enviarBase(resp) {
    this.formConvocatoria.patchValue({
      baseId: resp.baseId,
      convocatoriaId: resp.convocatoriaId,
      etapaId: resp.etapaId,
      desEtapa: resp.etapa,
      nomConvocatoria: resp.codConvAndCodRegimen,
      regimenId: resp.codProRegimen !== Const.MD_DL1041 ? '1' : '2',
      regimen: resp.codProRegimen,
      estadoConvocatoriaId: resp.estadoConvocatoriaId,
    });
  }

  initializeForm() {
    this.form = this.fb.group({
      baseId: 0,
      comunicadoId: 0,
      estado: '',
      codProgEstado: 0,
      etapaId: 0,
      desEtapa: '',
      nomConvocatoria: '',
      editable: false,
    });

    this.formConvocatoria = this.fb.group({
      baseId: 0,
      regimenId: 0,
      convocatoriaId: 0,
      etapaId: 0,
      desEtapa: '',
      nomConvocatoria: '',
      regimen: 0,
      estadoConvocatoriaId: 0,
    });
  }
}
