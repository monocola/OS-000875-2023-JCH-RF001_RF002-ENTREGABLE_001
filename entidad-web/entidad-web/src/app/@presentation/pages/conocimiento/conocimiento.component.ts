import { Component, OnInit } from '@angular/core';
import { Sort } from '@angular/material/sort';
import {
  setValidOrInvalidColors,
  sortDataTableComponent,
} from 'src/app/utils/general';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { FormBuilder, FormGroup } from '@angular/forms';
import { ModalRegistroConocimientoComponent } from './modal-registro-conocimiento/modal-registro-conocimiento.component';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from '../../@common-components/toast';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ConocimientoRepository } from 'src/app/@domain/repository/conocimiento.repository';

@Component({
  selector: 'serv-talento-conocimiento',
  templateUrl: './conocimiento.component.html',
  styleUrls: ['./conocimiento.component.scss'],
})
export class ConocimientoComponent implements OnInit {
  filterForm: FormGroup;
  conocimientos = [];
  conocimientoTableColumns: TableColumn[];
  searchMode = false;

  tipos = [];
  categoria = [];

  descripciones = [
    {
      id: 1,
      conocimiento: 'AMBIENTE',
    },
    {
      id: 2,
      conocimiento: 'DEPORTE',
    },
  ];
  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private toast: ToastService,
    private maestraService: MaestraRepository,
    private conocimientoService: ConocimientoRepository
  ) {}

  ngOnInit(): void {
    this.initializeColumns();
    this.getTipoConocimientos();
    this.getCategoriaConocimientos();
    this.initializeForm();
    this.getConocimientos();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      tipoConocimiento: '',
      categoria: '',
      descripcion: '',
    });
  }

  clear() {
    this.initializeForm();
    this.getConocimientos();
  }

  getConocimientos() {
    this.conocimientoService
      .getConocimiento(
        this.f.tipoConocimiento.value,
        this.f.categoria.value,
        this.f.descripcion.value
      )
      .subscribe((res) => {
        this.conocimientos = res;

        this.conocimientos = setValidOrInvalidColors(
          this.conocimientos,
          'estado'
        );

        this.conocimientos.forEach((element) => {
          element.estado = element.estado === '1' ? 'ACTIVO' : 'INACTIVO';
        });

        if (this.conocimientos.length !== 0) {
          this.searchMode = true;
        }
      });
  }

  getTipoConocimientos() {
    this.maestraService
      .getMaestraDetalleByCod('TBL_MAE_TIPO_CONO')
      .subscribe((res) => {
        this.tipos = res;
      });
  }

  getCategoriaConocimientos() {
    this.maestraService
      .getMaestraDetalleByCod('TBL_MAE_CATE_CONO')
      .subscribe((res) => {
        this.categoria = res;
      });
  }

  openModalCrearConocimiento() {
    this.OpenDialogAdd();
  }

  OpenDialogAdd() {
    const add = this.dialog.open(ModalRegistroConocimientoComponent, {
      disableClose: true,
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        this.getConocimientos();
      }
    });
  }

  editConocimiento(e) {
    const edit = this.dialog.open(ModalRegistroConocimientoComponent, {
      disableClose: true,
      data: e,
    });
    edit.afterClosed().subscribe((any) => {
      if (any) {
        this.getConocimientos();
      }
    });
  }

  removeConocimiento(e) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Desactivar conocimiento',
        bodyText:
          '¿Está seguro de querer deshabilitar el conocimiento seleccionado?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.conocimientoService
          .deleteConocimiento(e.maeConocimientoId)
          .subscribe(() => {
            this.getConocimientos();
            this.toast.showToast(
              'El conocimiento ha sido borrado exitosamente.',
              'success',
              'Atención'
            );
          });
      }
    });
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.conocimientos);
  }

  initializeColumns() {
    this.conocimientoTableColumns = [
      {
        name: '#',
        dataKey: 'maeConocimientoId',
        position: 'left',
        isSortable: true,
        width: '4%',
      },
      {
        name: 'TIPO DE CONOCIMIENTO',
        dataKey: 'descripcionTipo',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'CATEGORIA',
        dataKey: 'descripcionCategoria',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'DESCRIPCION',
        dataKey: 'descripcion',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'ESTADO',
        dataKey: 'estado',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de conocimientos SERVIR';
    model.headers = [
      '#',
      'TIPO DE CONOCIMIENTO',
      'CATEGORIA',
      'DESCRIPCION',
      'ESTADO',
    ];
    model.keys = [
      'maeConocimientoId',
      'descripcionTipo',
      'descripcionCategoria',
      'descripcion',
      'estado',
    ];
    return model;
  }
}
