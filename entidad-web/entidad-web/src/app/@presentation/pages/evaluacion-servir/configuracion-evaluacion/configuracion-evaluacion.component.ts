import { CdkDragDrop, moveItemInArray } from '@angular/cdk/drag-drop';
import { Component, OnInit } from '@angular/core';
import { FormControl } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { EvaluacionesServirRepository } from 'src/app/@domain/repository/evaluaciones-servir.repository';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EvServirComponentService } from '../evaluacion-servir.service';
const cloneDeep = require('lodash/cloneDeep');

@Component({
  selector: 'serv-talento-configuracion-evaluacion',
  templateUrl: './configuracion-evaluacion.component.html',
  styleUrls: ['./configuracion-evaluacion.component.scss'],
})
export class ConfiguracionEvaluacionComponent implements OnInit {
  treeNode: Node[] = [];
  regimenSelected = '';
  regimenControl = new FormControl('');
  showComboRegimen = false;

  showComboAddModalidad = false;
  newModalidadControl = new FormControl('');
  openModalidad = true;

  showComboAddTipoModalidad = false;
  tipoModalidadOpen: number = null;
  newTipoModalidadControl = new FormControl('');

  showComboAddEvaluacion = false;
  tipoModalidadCombo: number = null;
  subTipoModalidadCombo: number = null;
  setEvaluacionesControl = new FormControl([]);

  constructor(
    private toastService: ToastService,
    public helperServirService: EvServirComponentService,
    private EvaluacionesServirService: EvaluacionesServirRepository,
    private router: Router,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.helperServirService.loadCombox();
    if (this.helperServirService.editMode) {
      const data = { ...this.helperServirService.dataToEditBuilded };
      this.regimenSelected = data.regimenSelected;
      this.regimenControl.patchValue(data.regimenData);
      this.showComboRegimen = true;
      this.treeNode = data.treeNode.slice(0);
      this.verifyRow(0, 0, 0);
    } else {
      this.helperServirService.dataToEdit = null;
      this.helperServirService.dataToEditBuilded = null;
      this.helperServirService.editMode = false;
      this.helperServirService.createdElementToUpdate = false;
    }
  }

  addRegimen() {
    this.regimenSelected = this.regimenControl.value.descripcion;
    this.toastService.showToast('Régimen añadido correctamente', 'primary');
  }

  showRegimenCombo() {
    this.showComboRegimen = true;
  }

  newModality() {
    this.helperServirService.modalidadesFiltradas = this.helperServirService.modalidades;
    const controlesYaSeleccionados = this.treeNode.map((e) => e.control);
    const results = this.helperServirService.modalidadesFiltradas.filter(
      ({ maeDetalleId: id1 }) =>
        !controlesYaSeleccionados.some(({ maeDetalleId: id2 }) => id2 === id1)
    );
    this.helperServirService.modalidadesFiltradas = results.slice(0);
    this.showComboAddModalidad = true;
  }

  removeModalidad(index) {
    const removeDialog = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar tipo de modalidad',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    removeDialog.afterClosed().subscribe((res) => {
      if (res) {
        this.toastService.showToast(
          'Modalidad removida correctamente',
          'primary'
        );
        this.treeNode.splice(index, 1);
      }
    });
  }

  addModalidad() {
    this.treeNode.push({
      control: this.newModalidadControl.value,
      open: true,
      selected: this.newModalidadControl.value.descripcion,
      modalidades: [],
    });
    this.showComboAddModalidad = false;
    this.newModalidadControl.reset();
  }

  newModalityType(i) {
    this.tipoModalidadOpen = i;
    const regimenToSearch = this.helperServirService.evaluaciones.filter(
      (eva) => eva.maeDetalleId === this.regimenControl.value.maeDetalleId
    )[0];
    const modalidadToSearch =
      regimenToSearch?.listaDetalleModalidad.filter(
        (mod) => mod.maeDetalleId === this.treeNode[i].control.maeDetalleId
      )[0] || [];
    const listaTiposIdYaUsados =
      modalidadToSearch?.listaTipo?.map((e) => e.maeDetalleId) || [];
    const tiposToUse = this.helperServirService.tipos.map((tipo) => {
      if (!listaTiposIdYaUsados.includes(tipo.maeDetalleId)) {
        return tipo;
      }
    });

    this.helperServirService.tiposFiltrados = cloneDeep(
      tiposToUse.filter((t) => t)
    );
    const controlesYaSeleccionados = this.treeNode[i].modalidades.map(
      (e) => e.control
    );

    const results = this.helperServirService.tiposFiltrados.filter(
      ({ maeDetalleId: id1 }) =>
        !controlesYaSeleccionados.some(({ maeDetalleId: id2 }) => id2 === id1)
    );
    this.helperServirService.tiposFiltrados = results.slice(0);
    this.showComboAddTipoModalidad = true;
  }

  removeModalityType(i, j) {
    const removeDialog = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar tipo de modalidad',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    removeDialog.afterClosed().subscribe((res) => {
      if (res) {
        this.toastService.showToast(
          'Tipo de modalidad removido correctamente',
          'primary'
        );
        this.treeNode[i].modalidades.splice(j, 1);
      }
    });
  }

  // ----------------------------------------------------------------------
  // ---- MULTIPLE: Select de evaluaciones ------------------------------
  // ----------------------------------------------------------------------

  newEvaluaciones(i, j) {
    this.tipoModalidadCombo = i;
    this.subTipoModalidadCombo = j;
    this.showComboAddEvaluacion = true;
    if (this.treeNode[i].modalidades[j].evaluaciones.length > 0) {
      const aux = [];
      this.treeNode[i].modalidades[j].evaluaciones.forEach((eva) => {
        if (eva.estadoRegistro === '1') {
          aux.push(
            this.helperServirService.evaluacionesServir.filter(
              (item) => item.maeDetalleId === eva.tipoEvaluacionId
            )[0]
          );
        }
      });
      this.setEvaluacionesControl.patchValue(aux);
    }
  }

  verifyOption(i, j, item) {
    let exist = false;
    this.treeNode[i].modalidades[j].evaluaciones.forEach((eva) => {
      if (eva.tipoEvaluacionId === item.maeDetalleId) {
        exist = true;
      }
    });
    return exist;
  }

  addTipoModalidad(i) {
    this.treeNode[i].modalidades.push({
      control: this.newTipoModalidadControl.value,
      open: true,
      selected: this.newTipoModalidadControl.value.descripcionCorta,
      saved: false,
      evaluaciones: [],
      evaluacionesRemovidas: [],
    });
    this.showComboAddTipoModalidad = false;
    this.newTipoModalidadControl.reset();
  }

  // ----------------------------------------------------------------------
  // ---- Añadiendo nuevas evaluaciones ------------------------------
  // ----------------------------------------------------------------------

  addEvaluaciones(i, j) {
    const lenght = this.setEvaluacionesControl.value.length;
    this.setEvaluacionesControl.value.forEach((eva, index) => {
      let yaExiste = false;
      this.treeNode[i].modalidades[j].evaluaciones.forEach((item) => {
        if (item.tipoEvaluacionId === eva.maeDetalleId) {
          yaExiste = true;
        }
      });
      if (!yaExiste) {
        const newEvaluacion: EvaluacionTipoNode = {
          detalleEvaluacion: eva.descripcion,
          tipoEvaluacionId: eva.maeDetalleId,
          estadoRegistro: '1',
          jerarquiaId: this.helperServirService.editMode
            ? this.helperServirService.dataToEditBuilded.jerarquiaId
            : null,
          puntajeMaximo: 20,
          puntajeMinimo: 11,
          orden: null,
          estado: '1',
          evaluacionId: null,
          peso:
            lenght === index + 1
              ? Math.floor(-100 / lenght) * -1
              : ((-100 / lenght) >> 0) * -1,
          correcto: true,
          msgError: '',
        };
        this.treeNode[i].modalidades[j].evaluaciones.push(newEvaluacion);
      }
    });
    this.showComboAddEvaluacion = false;
    this.setEvaluacionesControl.patchValue([]);
    this.tipoModalidadCombo = null;
    this.subTipoModalidadCombo = null;
    this.verifyRow(i, j, 0);
  }

  drop(event: CdkDragDrop<string[]>, indexI, indexJ) {
    moveItemInArray(
      this.treeNode[indexI].modalidades[indexJ].evaluaciones,
      event.previousIndex,
      event.currentIndex
    );
  }

  saveEnlaces() {
    const { arrayBuilded, error } = this.transformArrayToSend();
    if (error === false) {
      this.EvaluacionesServirService.saveEnlaceRegimen(arrayBuilded).subscribe(
        (res) => {
          this.helperServirService.createdElementToUpdate = true;
          this.toastService.showToast(
            'El enlace se registró correctamente',
            'success'
          );
          this.setSavedStatusToModalidades();
        },
        (err) => {
          this.toastService.showToast(err.message, 'danger');
        }
      );
    } else {
      this.toastService.showToast(
        'Por favor complete los datos restantes',
        'danger'
      );
    }
  }

  saveEditEnlace() {
    const { arrayBuilded, error } = this.transformArrayToSend();
    if (error === false) {
      this.EvaluacionesServirService.editEnlaceRegimen(
        arrayBuilded[0].listaEvaluacion,
        this.helperServirService.dataToEditBuilded.jerarquiaId
      ).subscribe(
        (res) => {
          this.helperServirService.createdElementToUpdate = true;
          this.toastService.showToast(
            'El enlace se editó correctamente',
            'success'
          );
          this.router.navigateByUrl('pages/gestionevaluaciones');
        },
        (err) => {
          this.toastService.showToast(err.message, 'danger');
        }
      );
    } else {
      this.toastService.showToast(
        'Por favor complete los datos restantes',
        'danger'
      );
    }
  }

  removeEvaluacion(i, j, k) {
    this.showComboAddEvaluacion = false;
    if (this.treeNode[i].modalidades[j].evaluaciones[k].evaluacionId) {
      this.treeNode[i].modalidades[j].evaluaciones[k].estadoRegistro = '0';
      this.treeNode[i].modalidades[j].evaluacionesRemovidas.push(
        this.treeNode[i].modalidades[j].evaluaciones[k]
      );
      this.verifyRow(0, 0, 0);
    }
    this.treeNode[i].modalidades[j].evaluaciones.splice(k, 1);
    if (this.treeNode[i].modalidades[j].evaluaciones.length > 0) {
      this.verifyRow(i, j, k);
    }
  }

  transformArrayToSend() {
    const aux = [];
    let error = false;

    if (this.treeNode.length === 0) {
      error = true;
    }
    this.treeNode.forEach((modalidad) => {
      if (modalidad.modalidades.length === 0) {
        error = true;
      }
      modalidad.modalidades.forEach((tipoModalidad) => {
        const listaEvaluacion = [];
        if (
          tipoModalidad.evaluaciones.length === 0 ||
          tipoModalidad.correcto === false
        ) {
          error = true;
        }
        tipoModalidad.evaluaciones.forEach((eva, index) => {
          if (eva.correcto === false) {
            error = true;
          }
          listaEvaluacion.push({
            evaluacionId: eva.evaluacionId || null,
            jerarquiaId: eva.jerarquiaId || null,
            tipoEvaluacionId: eva.tipoEvaluacionId,
            orden: index + 1,
            peso: eva.peso,
            puntajeMinimo: eva.puntajeMinimo,
            puntajeMaximo: eva.puntajeMaximo,
            estado: eva.estadoRegistro,
          });
        });

        tipoModalidad.evaluacionesRemovidas.forEach((eva, index) => {
          listaEvaluacion.push({
            evaluacionId: eva.evaluacionId,
            jerarquiaId: eva.jerarquiaId,
            tipoEvaluacionId: eva.tipoEvaluacionId,
            orden: index + 1,
            peso: eva.peso,
            puntajeMinimo: eva.puntajeMinimo,
            puntajeMaximo: eva.puntajeMaximo,
            estado: '0',
          });
        });

        // ---------------- Seteando todo el arreglo ---------------- //
        if (!tipoModalidad.saved) {
          aux.push({
            codigoNivel1: this.regimenControl.value.maeDetalleId,
            codigoNivel2: modalidad.control.maeDetalleId,
            codigoNivel3: tipoModalidad.control.maeDetalleId,
            listaEvaluacion: listaEvaluacion,
          });
        }
      });
    });
    return {
      arrayBuilded: aux,
      error,
    };
  }

  setSavedStatusToModalidades() {
    this.treeNode.forEach((modalidad) => {
      modalidad.modalidades.forEach((tipoModalidad) => {
        tipoModalidad.saved = true;
      });
    });
  }

  verifyRow(i, j, k) {
    const evaluacion = this.treeNode[i].modalidades[j].evaluaciones[k];
    if (
      evaluacion &&
      evaluacion.tipoEvaluacionId &&
      evaluacion.peso >= 0 &&
      evaluacion.puntajeMinimo >= 0 &&
      evaluacion.puntajeMaximo >= 0
    ) {
      this.verifySuma100(i, j, k);
      if (evaluacion.puntajeMinimo <= evaluacion.puntajeMaximo) {
        evaluacion.msgError = '';
        this.verifySuma100(i, j, k);
      } else {
        evaluacion.correcto = false;
        evaluacion.msgError =
          'El puntaje máximo debe ser mayor al puntaje mínimo';
        this.verifySuma100(i, j, k);
      }
    } else {
      this.verifySuma100(i, j, k);
      // evaluacion?.correcto = false;
      // evaluacion.msgError =
      //   'Tiene que completar todos los campos obligatoriamente';
    }
  }

  verifySuma100(i, j, k) {
    const evaluacion = this.treeNode[i].modalidades[j].evaluaciones[k];
    let sumaPorcentual: number = 0;
    this.treeNode[i].modalidades[j].evaluaciones.forEach(
      (e) => (sumaPorcentual = sumaPorcentual + Number(e.peso))
    );
    if (sumaPorcentual === 100) {
      this.treeNode[i].modalidades[j].correcto = true;
      this.treeNode[i].modalidades[j].msgError = '';
      this.treeNode[i].modalidades[j].evaluaciones.forEach((eva) => {
        if (
          eva &&
          eva.tipoEvaluacionId &&
          eva.peso >= 0 &&
          eva.puntajeMinimo >= 0 &&
          eva.puntajeMaximo >= 0
        ) {
          if (eva.puntajeMinimo <= eva.puntajeMaximo) {
            eva.correcto = true;
            eva.msgError = '';
          } else {
            evaluacion.correcto = false;
            evaluacion.msgError =
              'El puntaje máximo debe ser mayor al puntaje mínimo';
          }
        } else {
          if (!eva.puntajeMinimo || !eva.puntajeMaximo) {
            eva.correcto = false;
            eva.msgError = 'Complete los campos faltantes';
          }
        }
      });
    } else {
      this.treeNode[i].modalidades[j].correcto = false;
      this.treeNode[i].modalidades[j].msgError =
        'El porcentaje total tiene que ser exactamente 100';
      this.treeNode[i].modalidades[j].evaluaciones.forEach(
        (eva) => (eva.correcto = false)
      );
    }
  }
}

export interface Node {
  control: any;
  open: boolean;
  selected: string;
  modalidades: TipoNode[];
}

export interface TipoNode {
  control: any;
  open: boolean;
  selected: string;
  saved?: boolean;
  evaluaciones: EvaluacionTipoNode[];
  evaluacionesRemovidas: EvaluacionTipoNode[];
  correcto?: boolean;
  msgError?: string;
}

export interface EvaluacionTipoNode {
  detalleEvaluacion: string;
  estado: string;
  estadoRegistro: string;
  evaluacionId: number;
  jerarquiaId: number;
  orden: null;
  peso: number | string;
  puntajeMaximo: number;
  puntajeMinimo: number;
  tipoEvaluacionId: number;
  correcto?: boolean;
  msgError?: string;
}
