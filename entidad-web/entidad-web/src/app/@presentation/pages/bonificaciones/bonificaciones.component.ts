import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Sort } from '@angular/material/sort';
import { BonificacionesRepository } from 'src/app/@domain/repository/bonificaciones.repository';
import { sortDataTableComponent } from 'src/app/utils/general';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { BonificacionesHelperService } from './bonificaciones-helper.service';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from '../../@common-components/toast';
import { BonificacionMensajesUser } from './bonificaciones-constantes';
import { Router } from '@angular/router';

@Component({
  selector: 'serv-talento-bonificaciones',
  templateUrl: './bonificaciones.component.html',
})
export class BonificacionesComponent implements OnInit {
  filterForm: FormGroup;
  bonificaciones = [];
  searchMode = false;
  bonificacionesTableColumns: TableColumn[];

  constructor(
    private fb: FormBuilder,
    private bonificacionesService: BonificacionesRepository,
    public helperService: BonificacionesHelperService,
    private dialog: MatDialog,
    private router: Router,
    private toast: ToastService,
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    this.helperService.loadCombox();
    this.getBonificaciones();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      titulo: '',
      tipoBonificacion: '',
      estado: '',
    });
  }

  clear() {
    this.initializeForm();
    this.getBonificaciones();
  }


  getBonificaciones() {
    this.searchMode = true;
    const body = this.filterForm.getRawValue();
    this.bonificacionesService.getBonificaciones(body).subscribe((res) => {
      this.bonificaciones = res;
    });
  }

  /** Para la tabla */

  editBonificacion(item) {
    this.router.navigateByUrl('pages/gestionbonificaciones/gestion/' + item.bonificacionId);
  }

  removeBonificacion(item) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: BonificacionMensajesUser.BON_ELIMINAR_MODAL_TITLE,
        bodyText: BonificacionMensajesUser.BON_ELIMINAR_MODAL_BODY,
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.bonificacionesService.deleteRegistro(item.bonificacionId).subscribe(() => {
          this.toast.showToast(BonificacionMensajesUser.BON_ELIMINAR_OK, 'danger', 'Atención');
          this.getBonificaciones();
        });
      }
    });
  }

  initializeColumns() {
    this.bonificacionesTableColumns = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '4%',
      },
      {
        name: 'TIPO DE BONIFICACIÓN',
        dataKey: 'nombreBonificacion',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'TITULO',
        dataKey: 'titulo',
        position: 'left',
        isSortable: true,
        width: '56%',
      },
      {
        name: 'ESTADO',
        dataKey: 'estadoDescripcion',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.bonificaciones);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de bonificaciones SERVIR';
    model.headers = ['#', 'TIPO DE BONIFICACIÓN', 'TÍTULO', 'ESTADO'];
    model.keys = [
      'bonificacionId',
      'nombreBonificacion',
      'titulo',
      'estadoDescripcion',
    ];
    return model;
  }
}
