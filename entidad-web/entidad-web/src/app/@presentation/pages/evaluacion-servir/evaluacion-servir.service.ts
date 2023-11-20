import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { EvaluacionesServirRepository } from 'src/app/@domain/repository/evaluaciones-servir.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { forkJoin } from 'rxjs';
import { Modalidad } from './../../../@data/model/evaluacion-servir/modalidad';
import { Tipo } from './../../../@data/model/evaluacion-servir/tipo';
import { Node } from './configuracion-evaluacion/configuracion-evaluacion.component';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';

@Injectable({
  providedIn: 'root',
})
export class EvServirComponentService {
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
    const getEvaluaciones = this.evaluacionServirService.getEvaluaciones();

    forkJoin([
      getRegimenes,
      getModalidades,
      getTipos,
      getEvaluacionesServir,
      getEstados,
      getEvaluaciones,
    ]).subscribe(
      (results) => {
        this.regimenes = results[0];
        this.modalidades = results[1];
        this.tipos = results[2];
        this.evaluacionesServir = results[3];
        this.estados = results[4];
        this.evaluaciones = results[5];
        this.handleEdit();
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

  handleEdit() {
    if (!this.editMode) {
      return;
    } else {
      const treeNode: Node[] = [
        {
          control: { ...this.dataToEdit.modalidad },
          open: true,
          selected: this.dataToEdit.modalidad.descripcionCorta,
          modalidades: [
            Object.assign(
              {},
              {
                control: this.dataToEdit.tipo,
                open: true,
                saved: false,
                selected: this.dataToEdit.tipo.descripcionCorta,
                evaluaciones: this.dataToEdit.tipo.listaEvaluacion.slice(0),
                evaluacionesRemovidas: [],
              }
            ),
          ],
        },
      ];
      this.dataToEditBuilded = {
        regimenSelected: this.dataToEdit.regimen.descripcionCorta,
        regimenData: { ...this.dataToEdit.regimen },
        jerarquiaId: treeNode[0].modalidades[0].evaluaciones[0].jerarquiaId,
        treeNode: treeNode.slice(0),
      };
    }
  }
}
