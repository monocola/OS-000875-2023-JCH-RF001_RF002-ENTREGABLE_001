import { Component, OnInit } from '@angular/core';
import {
  FormBuilder,
  FormGroup,
  FormControl,
  Validators,
} from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { GestoresOrhRepository } from '../../../@domain/repository/gestores-orh.repository';
import { forkJoin } from 'rxjs';
import { DatePipe } from '@angular/common';
import { GestoresOrh } from '../../../@data/model/gestores-orh';
import { ModalEliminarQuestionComponent } from './modal-eliminar-question/modal-eliminar-question.component';
import { ToastService } from '../../@common-components/toast';
import { ServidoresRepository } from '../../../@domain/repository/servidores.repository';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { Utils } from 'src/app/utils/utils';
import { Const } from '../../../@data/services/const';
import { ModalEditarOrhComponent } from './modal-editar-orh/modal-editar-orh.component';

@Component({
  selector: 'gme-web-gestores-orh',
  templateUrl: './gestores-orh.component.html',
  styleUrls: ['./gestores-orh.component.scss'],
})
export class GestoresOrhComponent implements OnInit {
  holderText =
    'Buscar por Tipo de documento , número de documento, nombres. apellidos...';
  profile = JSON.parse(sessionStorage.getItem('entidad'));
  const = Const.GESTOR_ID;
  searchMode = false;
  tableColumns: TableColumn[];
  filterForm: FormGroup = null;
  data: any[];
  dataEdit: any[];
  listGestores: GestoresOrh[] = [];
  tipoDocumento: MaestraParametro[];
  gestor: any;
  entidadId = this.profile.entidadId;

  simpleCharacteresAndNumber: string = 'integer';
  numeroDocumentoMaxlength: number = 8;

  constructor(
    private fb: FormBuilder,
    public dialog: MatDialog,
    private gestoresOrhRepository: GestoresOrhRepository,
    private datePipe: DatePipe,
    private toastService: ToastService,
    private servidoresRepository: ServidoresRepository
  ) {
    this.filterForm = this.fb.group({
      fechaNac: new FormControl(null, Validators.required),
    });
  }

  ngOnInit(): void {
    this.loadCombox();
    this.initializeColumns();
  }

  loadCombox() {
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    const getListGestor = this.gestoresOrhRepository.getList(this.entidadId);
    forkJoin([getTipoDocumento, getListGestor]).subscribe((results) => {
      this.tipoDocumento = results[0];
      this.listGestores = results[1];
    });
  }

  initializeColumns() {
    this.tableColumns = [
      {
        name: 'DNI/CE',
        dataKey: 'numeroDocumento',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Apellidos y Nombres',
        dataKey: 'nombreCompleto',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Correo electrónico',
        dataKey: 'correo',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Teléfono',
        dataKey: 'numeroTelefono',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Estado',
        dataKey: 'colorEstado',
        position: 'left',
        innerHtml: true,
        isSortable: true,
        width: '10%',
      },
    ];
  }

  delete(event) {
    const add = this.dialog.open(ModalEliminarQuestionComponent, {
      data: {
        title: 'Eliminar',
        bodyText:
          'Se eliminará el siguiente gestor registrado <br> ¿Está realmente seguro de realizar la siguiente acción ?',
        rutaImagen: './assets/images/question.png',
        textCancel: 'NO',
        textOk: 'SI',
      },
      disableClose: true,
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        console.info(any, event.gestorId);
        this.gestoresOrhRepository
          .deleteGestorOrh(event.gestorId)
          .subscribe((item) => {
            if (item) {
              this.toastService.showToast(
                'Se eliminó el siguiente usuario de manera exitosa',
                'success',
                'Eliminación exitosa'
              );
              this.ngOnInit();
            } else {
              this.toastService.showToast(
                'Ocurrio un error al eliminar al usuario',
                'danger',
                'Error'
              );
            }
          });
      }
    });
  }
  edit(events) {
    console.info(events);
    this.gestoresOrhRepository
      .getGestorId(events.gestorId)
      .subscribe((item) => {
        this.gestor = item.payload;
        console.info(this.gestor);
        const add = this.dialog.open(ModalEditarOrhComponent, {
          data: {
            gestorORH: this.gestor,
            isNew: false,
          },
          disableClose: true,
          width: '620px',
        });
        add.afterClosed().subscribe((any) => {
          if (any) {
            this.ngOnInit();
          }
        });
      });
  }

  registerManager = () => {
    const add = this.dialog.open(ModalEditarOrhComponent, {
      data: {
        isNew: true,
      },
      disableClose: true,
      width: '850px',
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        this.ngOnInit();
      }
    });
  }
}
