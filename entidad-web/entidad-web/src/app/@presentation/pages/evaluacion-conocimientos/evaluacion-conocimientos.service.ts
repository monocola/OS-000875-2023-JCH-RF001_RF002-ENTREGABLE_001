import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

@Injectable({
  providedIn: 'root',
})
export class EvaluacionConService {
  form: FormGroup;
  formProgramaciones: FormGroup;
  formResultadoExamen: FormGroup;
  preguntas = [];

  constructor(private fb: FormBuilder) {}

  enviarCategoria(resp) {
    this.form.patchValue({
      idcategoria: resp.categoriaId,
      categoria: resp.descripcion,
    });
    this.form.markAsPristine();
  }

  enviarProgramacion(resp) {
    this.formProgramaciones.patchValue({
      idConvocatoria: resp.convocatoriaId,
      idPerfil: resp.perfilId,
      convocatoria: resp.convocatoria,
    });
  }

  enviarProgramacionId(resp) {
    this.formProgramaciones.patchValue({
      programacionId: resp.programacionId,
    });
  }

  enviarDatosExamen(nombreExamen, examenId) {
    this.formProgramaciones.patchValue({
      examen: nombreExamen,
      examenId: examenId,
    });
  }

  initializeValues() {
    this.preguntas = [];
    this.initializeForm();
  }

  initializeForm() {
    this.form = this.fb.group({
      idcategoria: null,
      categoria: ['', Validators.required],
      txtDescripcion: '',
      txtTipoPregunta: '',
    });

    this.formProgramaciones = this.fb.group({
      idConvocatoria: null,
      idPerfil: null,
      convocatoria: '',
      programacionId: 0,
      examen: '',
      examenId: 0,
    });

    this.formResultadoExamen = this.fb.group({
      resultado: '',
      puntajeTotal: null,
      puntajeMax: null,
      puntajeMin: null,
    });
  }

  enviarDatosResultadoExamen(resultado, puntajeTotal, puntajeMax, puntajeMin) {
    this.formResultadoExamen.patchValue({
      resultado: resultado,
      puntajeTotal: puntajeTotal,
      puntajeMax: puntajeMax,
      puntajeMin: puntajeMin
    });
  }
}
