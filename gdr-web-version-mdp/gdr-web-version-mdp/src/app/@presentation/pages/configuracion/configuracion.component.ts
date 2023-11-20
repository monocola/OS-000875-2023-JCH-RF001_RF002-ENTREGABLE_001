import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { forkJoin } from 'rxjs';
import { Router } from '@angular/router';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { MatDialog } from '@angular/material/dialog';
import { AsignarEntidadComponent } from './asignar-entidad/asignar-entidad.component';
import { ModalNotificarComponent } from './modal-notificar/modal-notificar.component';
import { ConfiguracionRepository } from '../../../@domain/repository/configuracion.repository';
import { UsuarioGestor } from '../../../@data/model/usuariosGestores';
import { ModalNotificarComponentRector } from './modal-notificar-rector/modal-notificar-rector.component';

@Component({
  selector: 'serv-talento-configuracion',
  templateUrl: './configuracion.component.html',
  styleUrls: ['./configuracion.component.scss']
})
export class ConfiguracionComponent implements OnInit {

  frm: FormGroup = null;
  tableColumns: TableColumn[];
  data: any[] = [];
  listBody: null;

  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  rol = JSON.parse(sessionStorage.getItem('roles'));

  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private router: Router,
    private toastService: ToastService,
    private configuracionRepository: ConfiguracionRepository,
  ) { }

  cicloDefaultDesc;
  cicloDefault;
  cicloFilter: any;
  fecha = new Date();
  anioFilter = null;
  listaUsuarios = [];
  lstUsuarioRector: UsuarioGestor[] = [];
  fieldsize = 'medium';

  ngOnInit(): void {
    this.anioFilter = this.fecha.getFullYear();
    this.initForm();
    this.initializeColumns();
    this.loadCombox();
  }

  loadCombox() {
    const getListUsuarios = this.configuracionRepository.getListUsuariosServir();
    const getUsuarioRector = this.configuracionRepository.getUsuarioRector();
    forkJoin([getListUsuarios, getUsuarioRector]).subscribe(
      (results) => {
        this.listaUsuarios = results[0];

        this.lstUsuarioRector = results[1];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      userRector: new FormControl(null, Validators.required),
    });
  }

  get f() {
    return this.frm.controls;
  }

  initializeColumns() {
    this.tableColumns = [
      { name: 'Usuario Rector', dataKey: 'nombreCompleto', position: 'left', isSortable: true, width: '40%' },
      { name: 'Entidades asignadas', dataKey: 'siglaEntidad', position: 'left', isSortable: true, width: '35%' },
      { name: 'Nro Entidades asignadas', dataKey: 'conteoEntidad', position: 'left', isSortable: true, width: '10%' },
    ];
  }

  clear() {
    this.initForm();
    this.initializeColumns();
    this.loadCombox();
  }

 async buscar() {
    if (this.frm.valid) {

      const add = this.dialog.open(ModalNotificarComponentRector, {
        data: {
          rector: null
        },
        disableClose: true,
      });
      add.afterClosed().subscribe((any) => {

        if (any) {
          const body = {
            trace: {
              traceId: "string"
            },
            payload: {
              rolId: this.rol.rolId,
              personaId: this.frm.value.userRector.personaId,
              descripcionGDR: this.rol.nombreRol,
              flagActivado: '1'
            }
          };

          try { 
            this.configuracionRepository.asignaRector(body)
              .subscribe(result => {
                if (result) {
             
                  this.toastService.showToast('Se realizó la asignación de manera exitosa', 'success');
                  this.initForm();
                  this.initializeColumns();
                  this.loadCombox();
                } else {
                  this.toastService.showToast(result, 'danger');
                }
              });
          } catch (error) {
            this.toastService.showToast(error, 'danger');

          }

        }

      });




    } else {
      this.toastService.showToast('Seleccione un usuario rector', 'danger');
    }
  }

  asignar(event) {

    const add = this.dialog.open(AsignarEntidadComponent, {
      data: {
        rector: event
      },
      disableClose: true,
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        this.loadCombox();
      }
    });
  }

  notificar(rector) {

    if (rector.nroEntidades === 0) {
      this.toastService.showToast('No cuenta con entidades asignadas para notificar', 'info');
    } else {

      console.log("notifaf", rector);

      const add = this.dialog.open(ModalNotificarComponent, {
        data: {
          rector: rector
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
