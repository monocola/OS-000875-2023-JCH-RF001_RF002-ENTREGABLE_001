import { CdkDragDrop, moveItemInArray } from '@angular/cdk/drag-drop';
import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { EvaluacionesEntidadRepository } from 'src/app/@domain/repository/evaluaciones-entidad.repository';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EvaluacionEntidadService } from '../evaluacion-entidad.service';
import { ModalInfoEvaluacionesComponent } from '../modal-info-evaluaciones/modal-info-evaluaciones.component';

@Component({
  selector: 'serv-talento-administrar-evaluaciones',
  templateUrl: './administrar-evaluaciones.component.html',
  styleUrls: ['./administrar-evaluaciones.component.scss'],
})
export class AdministrarEvaluacionesComponent implements OnInit {
  bodySize = '62.5rem';
  evaluaciones: Evaluacion[] = [];
  evaluacionesEntidad = [];
  evaluacionesEntidadToDelete = [];

  correcto = false;
  msgError = '';

  jerarquiaId = null;

  constructor(
    public helperService: EvaluacionEntidadService,
    private maestraEntService: MaestraEntidadRepository,
    private evaluacionesService: EvaluacionesEntidadRepository,
    private toastService: ToastService,
    private router: Router,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    if (!this.helperService.dataToEdit) {
      this.router.navigateByUrl('/pages/gestionevaluaciones-entidad');
    } else {
      this.getEvaluacionesEntidad();
    }
  }

  save() {
    let someError = false;
    this.evaluaciones.forEach((e) => {
      if (!e.correcto) {
        someError = true;
      }
    });
    if (!someError) {
      this.evaluacionesService
        .updateEvaluacionEntidad(
          this.evaluaciones.concat(this.evaluacionesEntidadToDelete),
          this.jerarquiaId
        )
        .subscribe((res) => {
          this.toastService.showToast(
            'Las evaluaciones se actualizaron con éxito',
            'primary'
          );
          this.router.navigateByUrl('/pages/gestionevaluaciones-entidad');
        });
    } else {
      this.toastService.showToast(
        'Complete los campos faltantes y/o erróneos',
        'danger'
      );
    }
  }

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  getEvaluacionesWRepeat(order) {
    const idsSelected: number[] = this.evaluaciones.map(
      (e) => e.tipoEvaluacionId || null
    );
    if (this.evaluaciones[order].tipoEvaluacionId) {
      const index = idsSelected.indexOf(
        this.evaluaciones[order].tipoEvaluacionId
      );
      idsSelected.splice(index, 1);
      return this.evaluacionesEntidad.filter(
        (b) => !idsSelected.includes(b.maeDetalleEntidadId)
      );
    } else {
      return this.evaluacionesEntidad.filter(
        (b) => !idsSelected.includes(b.maeDetalleEntidadId)
      );
    }
  }

  addEvaluacion() {
    this.evaluaciones = [
      ...this.evaluaciones,
      {
        correcto: false,
        detalleEvaluacion: '',
        estado: '1',
        evaluacionEntidad: null,
        evaluacionId: null,
        jerarquiaId: 23,
        msgError: '',
        orden: this.evaluaciones.length + 1,
        peso: 0,
        evaluacionOrigenId: null,
        puntajeMaximo: 20,
        puntajeMinimo: 11,
        tipoEvaluacionId: null,
      },
    ];
    this.verifyRow(0);
  }

  getEvaluacionesEntidad() {
    const getMaestraCursoDetalleEnt = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_EVALUACION'
    );
    getMaestraCursoDetalleEnt.subscribe((res) => {
      this.evaluacionesEntidad = res;
      this.evaluaciones = this.helperService.dataToEdit.tipo.listaEvaluacion;
      this.jerarquiaId = this.helperService.dataToEdit.tipo.jerarquia;
      this.verifyRow(0);
    });
  }

  removeEvaluacion(index) {
    const eva: Evaluacion = this.evaluaciones[index];
    if (eva.evaluacionEntidad) {
      eva.estado = '0';
      this.evaluacionesEntidadToDelete = [
        ...this.evaluacionesEntidadToDelete,
        eva,
      ];
    }
    this.evaluaciones.splice(index, 1);
    this.verifyRow(0);
  }

  drop(event: CdkDragDrop<string[]>) {
    moveItemInArray(this.evaluaciones, event.previousIndex, event.currentIndex);
  }

  openModalInfo() {
    this.dialog.open(ModalInfoEvaluacionesComponent);
  }

  verifyRow(k) {
    const eva = this.evaluaciones[k];
    if (
      eva &&
      eva.tipoEvaluacionId &&
      eva.peso >= 0 &&
      eva.puntajeMinimo >= 0 &&
      eva.puntajeMaximo >= 0
    ) {
      if (eva.puntajeMinimo <= eva.puntajeMaximo) {
        eva.msgError = '';
      } else {
        eva.correcto = false;
        eva.msgError = 'El puntaje máximo debe ser mayor al puntaje mínimo';
      }
    } else {
      if (!eva.puntajeMinimo || !eva.puntajeMaximo) {
        eva.correcto = false;
        eva.msgError = 'Complete los campos faltantes';
      }
      if (!this.evaluaciones[k].tipoEvaluacionId) {
        eva.correcto = false;
        eva.msgError = 'Debe seleccionar una evaluación';
      }
    }
    this.verifySuma100();
  }

  verifySuma100() {
    let sumaPorcentual: number = 0;
    this.evaluaciones.forEach(
      (e) => (sumaPorcentual = sumaPorcentual + Number(e.peso))
    );
    if (sumaPorcentual === 100) {
      this.correcto = true;
      this.msgError = '';
      this.evaluaciones.forEach((eva, index) => {
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
            this.correcto = false;
            this.msgError =
              'El puntaje máximo debe ser mayor al puntaje mínimo';
          }
        } else {
          if (!this.evaluaciones[index].tipoEvaluacionId) {
            this.evaluaciones[index].correcto = false;
            this.evaluaciones[index].msgError =
              'Debe seleccionar una evaluación';
          }

          if (
            !this.evaluaciones[index].puntajeMinimo ||
            !this.evaluaciones[index].puntajeMaximo
          ) {
            this.evaluaciones[index].correcto = false;
            this.evaluaciones[index].msgError = 'Complete los campos faltantes';
          }
        }
      });
    } else {
      this.correcto = false;
      this.msgError = 'El porcentaje total tiene que ser exactamente 100';
      this.evaluaciones.forEach((eva) => (eva.correcto = false));
    }
  }
}

export interface Evaluacion {
  correcto: boolean;
  detalleEvaluacion: string;
  estado: string;
  evaluacionEntidad: number;
  evaluacionId: number;
  jerarquiaId: number;
  orden: number;
  peso: number;
  msgError: string;
  evaluacionOrigenId: number;
  puntajeMaximo: number;
  puntajeMinimo: number;
  tipoEvaluacionId: number;
}
