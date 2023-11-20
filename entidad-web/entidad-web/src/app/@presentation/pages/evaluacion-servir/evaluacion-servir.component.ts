import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { EvaluacionesServirRepository } from 'src/app/@domain/repository/evaluaciones-servir.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EvServirComponentService } from './evaluacion-servir.service';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { Observable } from 'rxjs';
import { MatDialog } from '@angular/material/dialog';
import cloneDeep from 'lodash/cloneDeep';

@Component({
  selector: 'serv-talento-evaluacion-servir',
  templateUrl: './evaluacion-servir.component.html',
})
export class EvaluacionServirComponent implements OnInit {
  evaluaciones: any[] = [];

  regimenes: any[] = [];
  modalidades: any[] = [];
  tipos: any[] = [];
  estados: any[] = [];

  constructor(
    private evaluacionServirService: EvaluacionesServirRepository,
    public evServirService: EvServirComponentService,
    private toastService: ToastService,
    private router: Router,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.evServirService.initializeForm();
    this.evServirService.loadCombox();
    if (this.evServirService.createdElementToUpdate) {
      this.evServirService.updateTable();
      this.evServirService.createdElementToUpdate = false;
    }
  }

  get f() {
    return this.evServirService.filterForm.controls;
  }

  clear() {
    this.evServirService.initializeForm();
    this.evServirService.updateTable();
  }

  search() {
    const body = this.evServirService.filterForm.value;
    this.evaluacionServirService.searchEnlacesRegimen(body).subscribe(
      (res) => {
        this.evServirService.evaluaciones = res;
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }

  edit(row) {
    if (row.level === 2) {
      const aux = cloneDeep(row.data.restOfData);
      this.evServirService.dataToEdit = { ...aux };
      this.evServirService.editMode = true;
      this.evServirService.handleEdit();
      this.router.navigateByUrl('pages/gestionevaluaciones/configuracion');
    }
  }

  delete(row) {
    const deleteConfirmation = this.dialog.open(ModalConfirmationComponent, {
      data: {},
    });
    deleteConfirmation.afterClosed().subscribe((res) => {
      if (res) {
        let deleteJerarquia: Observable<any> = null;
        switch (row.level) {
          case 0:
            deleteJerarquia = this.evaluacionServirService.deleteJerarquia(
              row.data.restOfData.codigoNivel1
            );
            break;
          case 1:
            deleteJerarquia = this.evaluacionServirService.deleteJerarquia(
              row.data.restOfData.regimen.codigoNivel1,
              row.data.restOfData.modalidad.codNivel2
            );
            break;
          case 2:
            deleteJerarquia = this.evaluacionServirService.deleteJerarquia(
              row.data.restOfData.regimen.codigoNivel1,
              row.data.restOfData.modalidad.codNivel2,
              row.data.restOfData.tipo.codNivel3
            );
            break;
          default:
            break;
        }
        deleteJerarquia.subscribe(
          () => {
            this.evServirService.updateTable();
          },
          (err) => {
            this.toastService.showToast(err, 'danger');
          }
        );
      }
    });
  }

  enlazarRegimen() {
    this.evServirService.initializarValues();
    this.router.navigateByUrl('pages/gestionevaluaciones/configuracion');
  }
}
