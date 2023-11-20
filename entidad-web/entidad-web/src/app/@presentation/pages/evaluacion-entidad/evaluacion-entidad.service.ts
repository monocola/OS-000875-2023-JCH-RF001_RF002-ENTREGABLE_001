import { FormBuilder, FormGroup } from '@angular/forms';
import { Injectable } from '@angular/core';
import { EvaluacionesServirRepository } from 'src/app/@domain/repository/evaluaciones-servir.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { Modalidad } from './../../../@data/model/evaluacion-servir/modalidad';
import { forkJoin } from 'rxjs';
import { Tipo } from './../../../@data/model/evaluacion-servir/tipo';

@Injectable({
  providedIn: 'root',
})
export class EvaluacionEntidadService {
  constructor(
    private evaluacionServirService: EvaluacionesServirRepository,
    private parameterService: ParameterRepository,
    private fb: FormBuilder
  ) {}

  filterForm: FormGroup;

  evaluaciones = [];
  regimenes: DetalleMaestra[] = [];
  estados = [];

  evaluacionesServir: Modalidad[] = [];
  evaluacionesServirFiltradas = [];

  modalidades: Modalidad[] = [];
  modalidadesFiltradas: Modalidad[] = [];

  tipos: Tipo[] = [];
  tiposFiltrados: Tipo[] = [];

  dataToEdit = null;
  dataToEditBuilded = null;
  editMode = false;
  createdElementToUpdate = false;

  updateTable() {
    const getEvaluaciones = this.evaluacionServirService.getEvaluaciones();
    getEvaluaciones.subscribe((res) => {
      this.evaluaciones = res;
    });
  }

  initializarValues() {
    this.dataToEdit = null;
    this.dataToEditBuilded = null;
    this.editMode = false;
    this.createdElementToUpdate = false;
  }

  loadCombox() {
    const getRegimenes = this.evaluacionServirService.getRegimenesServir('TBL_REGIMEN');
    const getModalidades = this.evaluacionServirService.getModalidadesServir('TBL_MODALIDAD');
    const getTipos = this.evaluacionServirService.getTiposServir('TBL_TIPO');
    const getEvaluacionesServir = this.evaluacionServirService.getEvaluacionesServir('TBL_EVALUACION');
    const getEstados = this.parameterService.getEstadoRegistro();
    // const getEvaluaciones = this.evaluacionServirService.getEvaluaciones();

    forkJoin([
      getRegimenes,
      getModalidades,
      getTipos,
      getEvaluacionesServir,
      getEstados,
      // getEvaluaciones,
    ]).subscribe(
      (results) => {
        this.regimenes = results[0];
        this.modalidades = results[1];
        this.tipos = results[2];
        this.evaluacionesServir = results[3];
        this.estados = results[4];
        // this.evaluaciones = results[5];
        // this.handleEdit();
      },
      (err) => {}
    );
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      regimen: '',
      modalidad: '',
      tipo: '',
      estado: '',
    });
  }
}
