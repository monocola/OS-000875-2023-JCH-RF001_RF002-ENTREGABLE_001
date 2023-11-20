import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { EvaluacionesServirRepository } from 'src/app/@domain/repository/evaluaciones-servir.repository';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { EvaluacionEntidadService } from './evaluacion-entidad.service';
import { ModalJerarquiaComponent } from './modal-jerarquia/modal-jerarquia.component';
import { Router } from '@angular/router';
import { ToastService } from '../../@common-components/toast';
import { EvaluacionesEntidadRepository } from 'src/app/@domain/repository/evaluaciones-entidad.repository';
import cloneDeep from 'lodash/cloneDeep';

@Component({
  selector: 'serv-talento-evaluacion-entidad',
  templateUrl: './evaluacion-entidad.component.html',
})
export class EvaluacionEntidadComponent implements OnInit {
  evaluacionesEntidad = [];
  jerarquias = [];

  constructor(
    public helperService: EvaluacionEntidadService,
    private dialog: MatDialog,
    private evaluacionesServirService: EvaluacionesServirRepository,
    private evaluacionesEntidadService: EvaluacionesEntidadRepository,
    private authService: AuthenticationRepository,
    private router: Router,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.helperService.initializeForm();
    this.helperService.loadCombox();
    this.getEvaluacionesEntidad();
  }

  get f() {
    return this.helperService.filterForm.controls;
  }

  getEvaluacionesEntidad(form?) {
    const body = {
      entidadId: this.authService.getCurrentUserValue.entidadId,
      regimen: form?.regimen || null,
      modalidad: form?.modalidad || null,
      tipo: form?.tipo || null,
    };
    this.evaluacionesServirService
      .searchEnlacesRegimen(body)
      .subscribe((res) => {
        this.evaluacionesEntidad = [...res];
        if (res.length === 0) {
          this.jerarquias = [];
        }
      });
  }

  clear() {
    this.helperService.initializeForm();
    this.getEvaluacionesEntidad(this.helperService.filterForm.getRawValue());
  }

  search() {
    this.getEvaluacionesEntidad(this.helperService.filterForm.getRawValue());
  }

  jerarquiasExistentes(data) {
    this.jerarquias = [...data];
  }

  edit(row) {
    if (row.level === 2) {
      const aux = cloneDeep(row.data.restOfData);
      this.helperService.dataToEdit = { ...aux };
      this.helperService.editMode = true;
      // this.helperService.handleEdit();
      this.router.navigateByUrl('pages/gestionevaluaciones-entidad/edicion');
    }
  }

  delete(row) {
    const jerarquiaId = row.data.restOfData.tipo.jerarquia;
    const deleteConfirmation = this.dialog.open(ModalConfirmationComponent);
    deleteConfirmation.afterClosed().subscribe((accepted) => {
      if (accepted) {
        this.evaluacionesEntidadService
          .eliminarJerarquia(jerarquiaId)
          .subscribe((res) => {
            this.toastService.showToast(
              'La evaluación ha sido desactivada con éxito',
              'success'
            );
            this.getEvaluacionesEntidad(
              this.helperService.filterForm.getRawValue()
            );
          });
      }
    });
  }

  openModalHierarchy() {
    const newJerarquia = this.dialog.open(ModalJerarquiaComponent, {
      width: '1100px',
      maxWidth: '1150px',
      maxHeight: '45rem',
      data: {
        jerarquiasExistentes: this.jerarquias,
      },
    });
    newJerarquia.afterClosed().subscribe((res) => {
      if (res) this.clear();
    });
  }
}
