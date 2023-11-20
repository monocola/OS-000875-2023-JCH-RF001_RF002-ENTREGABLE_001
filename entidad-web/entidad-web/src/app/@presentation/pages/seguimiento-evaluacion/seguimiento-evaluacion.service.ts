import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Const } from '../../../@data/services/const';

@Injectable({
  providedIn: 'root',
})
export class SeguimientoEvaluacionService {
  form: FormGroup;
  formConvocatoria: FormGroup;

  constructor(private fb: FormBuilder) {}


  initializeValues() {
    this.initializeForm();
  }

  enviarBase(resp) {
    this.formConvocatoria.patchValue({
      baseId: resp.baseId,
      convocatoriaId: resp.convocatoriaId,
      etapaId: resp.etapaId,
      desEtapa: resp.etapa,
      nomConvocatoria: resp.codConvAndCodRegimen,
      regimenId: resp.codProRegimen !== Const.MD_DL1041 ? '1' : '2',
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
    });
  }

}
