import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { NbDialogService } from '@nebular/theme';
import { DetalleGestorGdrComponent } from './detalle.gestor.gdr/detalle-gestor-gdr.component';
import { NotificarGdrComponent } from './notificar-gdr/notificar-gdr.component';
import { Const } from 'src/app/@data/services/const';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ToastService } from '../../@common-components/toast';
import { ImplementacionRepository } from 'src/app/@domain/repository/implementacion.repository';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ConfiguracionRepository } from 'src/app/@domain/repository/configuracion.repository';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { FileVisualizerComponent } from '../../@common-components/file-visualizer/file-visualizer.component';

@Component({
  selector: 'serv-talento-asignar-gestor-gdr',
  templateUrl: './asignar-gestor-gdr.component.html',
  styleUrls: ['./asignar-gestor-gdr.component.scss']
})

export class AsignarGestorGdrComponent implements OnInit {

  entidadId: number;
  entidad: any;
  searchMode = false;
  const = Const;
  tableColumns: TableColumn[];
  currentCicle: string = '';
  fileBase64 = null;
  dataToEdit: any = null;
  dataPDF = null;

  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private router: Router,
    private parameterRepository: ParameterRepository,
    private toastService: ToastService,
    private implementacionRepository: ImplementacionRepository,
    private authenticationService: AuthenticationRepository,
    private activatedRoute: ActivatedRoute,
    private dialogService: NbDialogService,
    private configuracionRepository: ConfiguracionRepository,
  ) {
    this.authenticationService.getCurrentCiclo()
      .subscribe(value => {
        if (value != null) {
          this.currentCicle = value;
        }
      });
   }
    frm: FormGroup = null;
    cicloDefaultDesc;
    cicloDefault;
    sector: MaestraParametro[];
    nivel: MaestraParametro[];
    cicloFilter: any;
    tipoEntidad: MaestraParametro[];
    fecha = new Date();
    anioFilter = null;
    listaEntidadesFiltradas = [];
    listaEntidades = [];
    listaUsuariosGdr = [];
    lstImplementacion = [];
    data: any[] = [];
    listBody: null;

  get f() {
    return this.frm.controls;
  }

  files: File[] = [];

  addEntidades() {
  }

  agregarGestor(idGestor) {
      this.dialogService
        .open(DetalleGestorGdrComponent, {
          context: {
            idGestor: idGestor,
            isEdit: idGestor != null,
            entidadId: this.entidadId,
          },
          closeOnBackdropClick: false,
          autoFocus: false,
          closeOnEsc: false,
        })
        .onClose.subscribe(async (result: boolean) => {
         if (result) {
           this.listaUsuariosGdr = await this.implementacionRepository.getListUsuariosGdr(this.entidadId).toPromise();
         }
      });
  }

  clear() {
    this.loadCombox();
    this.initForm();
  }

  ngOnInit(): void {
    this.activatedRoute.params.subscribe((params: Params) => {
      if (params.entidadId) {
        this.entidadId = params.entidadId;
      }
      if (params.entidad) {
        this.entidad = params.entidad;
      }
    });
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
  }

  initializeColumns() {
    this.tableColumns = [
      { name: 'N° documento', dataKey: 'nroDocumento', position: 'left', isSortable: true, innerHtml: true, width: '10%' },
      { name: 'Usuario GDR registrado', dataKey: 'nombreCompleto', position: 'left', isSortable: true , width: '15%'},
      { name: 'Puesto asignado', dataKey: 'puesto', position: 'left', isSortable: true, innerHtml: true, width: '15%' },
      { name: 'Fecha inicio', dataKey: 'fechaInicio', position: 'left', isSortable: true, width: '15%' },
      { name: 'Fecha fin', dataKey: 'fechaFin', position: 'left',  isSortable: true, width: '15%' },
      { name: 'Estado', dataKey: 'colorEstado', position: 'left', innerHtml: true,   isSortable: true, width: '10%' },
    ];
  }

  loadCombox() {
    const getListEntidad = this.implementacionRepository.getListEntidades();
    const getListUsuariosGdr = this.implementacionRepository.getListUsuariosGdr(this.entidadId);
    forkJoin([ getListEntidad, getListUsuariosGdr]).subscribe(
      (results) => {
        this.listaEntidades = results[0];
        console.log("entidades:", this.listaEntidades)
        this.listaUsuariosGdr = results[1];
        console.info("this.entidadId:",this.entidadId);
        //console.info("entidad:",this.listaEntidades.filter(x=> x.entidadId === this.entidadId));

       // console.info("entidad:",this.listaEntidades.find(({ entidadId }) => entidadId == 1));

        this.entidad = this.listaEntidades.find( x=> x.entidadId == this.entidadId);
        console.log("thiss:",this.entidad)
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      entidad: '',
    });
  }

  editUser(row) {
    this.agregarGestor(row.gestorEntidadId);
  }

  actionShowPDF(events) {
    this.dataPDF = this.implementacionRepository.getObtenerPDFs(events.uuId)
      .subscribe(res => {
        this.fileBase64 = res;
        this.dialog.open(FileVisualizerComponent, {
          data: {
            base64String: this.fileBase64,
            filename: events.etapa,
            extension: 'pdf',
          },
        });
      });
  }

  notificar(gestor) {

    if ( gestor.estadoRegistro === '0' ) {
      this.toastService.showToast('EL usuario no se encuentra activo', 'info');

    } else {
      const add = this.dialog.open(NotificarGdrComponent, {
        data: {
          gestor: gestor
        },
        disableClose: true,
      });
      add.afterClosed().subscribe((any) => {
        if (any) {
          this.configuracionRepository.notificaUsuarioReg(any)
            .subscribe(response => {
              if (response.payload.estado) {
                this.toastService.showToast(response.payload.mensaje, 'success');
                this.loadCombox();
              } else {
                this.toastService.showToast(response.status.error.messages[0].message, 'danger');
                this.loadCombox();
              }
            });
        }
      });
    }
  }

}
