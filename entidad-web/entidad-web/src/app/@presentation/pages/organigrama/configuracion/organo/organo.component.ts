import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Organo } from 'src/app/@data/model/organo';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { sortDataTableComponent } from 'src/app/utils/general';

@Component({
  selector: 'serv-talento-organo',
  templateUrl: './organo.component.html',
  styleUrls: ['./organo.component.scss'],
})
export class OrganoComponent implements OnInit {
  @Input() createMode = false;
  @Input() editDataFromGraph = null;
  @Output() closeOrgano = new EventEmitter();
  @Output() editOrganoEmmiter = new EventEmitter();

  editMode = false;
  dataToEdit: Organo = null;
  ordersTableColumns: TableColumn[];
  organos: Organo[] = [];
  index = 0;

  constructor(
    public organosRepository: OrganoRepository,
    private toastService: ToastService,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.getOrganos(false);
    this.initializeColumns();
  }

  handleClose(flag) {
    this.closeOrgano.emit(flag);
    if (flag === 1) {
      this.dataToEdit = null;
      this.createMode = false;
    } else {
      this.index = 0;
      this.editMode = false;
    }
  }

  handleUpdateEmitter() {
    this.getOrganos(true);
    this.handleClose(1);
  }

  getOrganos(updateValues: boolean) {
    this.organosRepository.getOrganos(updateValues).subscribe(
      (res) => {
        // this.loading = false;
        this.organos = res;
        this.organos.forEach((organo) => {
          if (organo.estado === 'INACTIVO') {
            // organo.disableDelete = true;
            organo.settings = {};
            organo.settings.disableDelete = true;
            organo.settings.estado = {};
            organo.settings.estado.color = '#EB5757';
          } else {
            organo.settings = {};
            organo.settings.estado = {};
            organo.settings.estado.color = '#0d88bc';
          }
        });
      },
      (err) => {
        // this.loading = false;
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  editOrgano(organo: any) {
    window.scrollTo(0, 0);
    this.dataToEdit = organo;
    this.createMode = true;
    this.editMode = true;
    this.editOrganoEmmiter.emit(true);
  }

  removeOrgano(organo: any) {
    const deleteSede = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Desactivar órgano',
        bodyText: '¿Está seguro de querer deshabilitar el órgano seleccionado?',
      },
    });
    deleteSede.afterClosed().subscribe((res) => {
      if (res === true) {
        this.organosRepository.deleteOrgano(organo.organigramaId).subscribe(
          (organoDeleted) => {
            this.toastService.showToast(
              'El órgano se ha deshabilitado correctamente',
              'success'
            );
            this.getOrganos(true);
          },
          (err) => {
            this.toastService.showToast(
              err || 'Ocurrió un error al inactivar un órgano',
              'danger'
            );
          }
        );
      }
    });
  }

  initializeColumns() {
    this.ordersTableColumns = [
      {
        name: '#',
        dataKey: 'organigramaId',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'NIVEL',
        dataKey: 'desNivel',
        position: 'left',
        isSortable: true,
      },
      { name: 'SIGLA', dataKey: 'sigla', position: 'left', isSortable: true },
      {
        name: 'ÓRGANO',
        dataKey: 'descripcion',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'NATURALEZA',
        dataKey: 'desNaturaleza',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'DEPENDENCIA JERÁRQUICA',
        dataKey: 'descripOrganoPadre',
        position: 'left',
        isSortable: true,
      },
      { name: 'ESTADO', dataKey: 'estado', position: 'left', isSortable: true },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.organos);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de entidades';
    model.headers = [
      'ID',
      'NIVEL',
      'SIGLA',
      'ÓRGANO',
      'NATURALEZA',
      'DEPENDENCIA JERÁRQUICA',
      'TIPO DOCUMENTO',
      'NUMERO DOCUMENTO',
      'NOMBRES',
      'AP. PATERNO',
      'AP. MATERNO',
      'PAÍS',
      'PUESTO',
      'CELULAR',
      'CORREO LABORAL',
      'ESTADO',
    ];
    model.keys = [
      'organigramaId',
      'desNivel',
      'sigla',
      'descripcion',
      'desNaturaleza',
      'descripOrganoPadre',
      'destipoDocumento',
      'nroDocumento',
      'nombres',
      'apellidoPaterno',
      'apellidoMaterno',
      'nombrePais',
      'puesto',
      'telefono',
      'correo',
      'estado',
    ];
    return model;
  }
}
