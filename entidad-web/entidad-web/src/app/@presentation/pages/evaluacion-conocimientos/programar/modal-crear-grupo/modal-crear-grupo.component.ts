import { Component, Inject, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin } from 'rxjs';
import { ThemePalette } from '@angular/material/core';
import { Ubigeo } from 'src/app/@data/model/ubigeo';
import { Utils } from 'src/app/utils/utils';
import moment from 'moment/moment';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-modal-crear-grupo',
  templateUrl: './modal-crear-grupo.component.html',
  styleUrls: ['./modal-crear-grupo.component.scss'],
})
export class ModalCrearGrupoComponent implements OnInit {
  @ViewChild('picker') picker: any;

  public color: ThemePalette = 'primary';
 
  tipoExamen = '1';
 
  cmbModalidad = [];
  cmbSede = [];
  autoEvaluaciones = [];
  autoEvaluadores = [];
  isVirtual: boolean = true;
  isEditar = null;
  isOtroSelected = false;
  titulo: any;
  nombreperfil: any;
  cantidadPostulantes: any;
  cantGrupos: any;
  nombreGrupo: any;
  perfilId: any;
  idConvocatoria: any;
  programacionData: any;

  departamentos: Ubigeo[] = [];
  provincias: Ubigeo[] = [];
  distritos: Ubigeo[] = [];

  idDpto: any;
  idProv: any;
  idDistr: any;

  programacionId: any = null;
  totalPostulantesConvocados: number = 0;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalCrearGrupoComponent>,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private toast: ToastService,
    private parametrosRepository: ParameterRepository,
    public authenticationRepository: AuthenticationRepository,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  registerForm: FormGroup = this.fb.group({
    evaluacion: [null, Validators.required],
    evaluador: [null, Validators.required],
    cantidad: [null, Validators.required],
    selModalidad: ['', Validators.required],
    selSede: '',
    indicaciones: '',
    linkMeet: '',
    referencias: '',
    fechaInicio: ['', Validators.required],
    fechaFin: ['', Validators.required],
    deptSelectValue: null,
    provSelectValue: null,
    distSelectValue: null,
    lugarEvaluacion: null,
    nombreSede: null,
  });

  get f() {
    return this.registerForm.controls;
  }

  ngOnInit(): void {
    console.log (this.data);
    this.dialogRef.updateSize('60%', '95%');
    this.titulo = this.data.titulo;
    this.nombreperfil = this.data.nombreperfil;
    this.cantidadPostulantes = this.data.cantidadPostulantes;
    this.cantGrupos = this.data.cantGrupos + 1;
    this.nombreGrupo = 'GRUPO ' + (this.data.cantGrupos + 1);
    this.perfilId = this.data.perfilId;
    this.idConvocatoria = this.data.idConvocatoria;
    this.isEditar = this.data.isEditar;

    this.perfilId = this.data.perfilId;
    this.idConvocatoria = this.data.idConvocatoria;
    this.totalPostulantesConvocados = this.data.totalPostulantesConvocados;

    forkJoin([
      [this.getModalidadesEvaluacion()],
      [this.getSede()],
      [this.getEvaluadoresData()],
      [this.getEvaluaciones()],
      [this.getDepartamentosData()],
    ]).subscribe((res) => {});

    if (this.isEditar) {
      this.f.referencias.setValue(this.data.programacionData.referencia);
      this.f.indicaciones.setValue(this.data.programacionData.indicaciones);
      this.f.linkMeet.setValue(this.data.programacionData.link);
      this.programacionId = this.data.programacionData.programacionId;

      this.f.selModalidad.setValue(this.data.programacionData.modalidadId);

      this.f.selSede.setValue(this.data.programacionData.sedeEvaluacionesId);

      this.nombreGrupo = this.data.programacionData.nombreGrupo;
      this.f.fechaInicio.setValue(
        Utils.parseFechaString(this.data.programacionData.fechaInicioExamen)
      );
      this.f.fechaFin.setValue(
        Utils.parseFechaString(this.data.programacionData.fechaFinExamen)
      );

      this.f.cantidad.setValue(this.data.programacionData.cantidad);
    }
  }

  private getEvaluaciones() {
    this.evaluacionConocimientosService
      .listarExamenesProgramacion()
      .subscribe((res) => {
        this.autoEvaluaciones = res;

        if (this.isEditar) {
          if (
            this.autoEvaluaciones != null &&
            this.autoEvaluaciones.length > 0
          ) {
            this.autoEvaluaciones.forEach((element) => {
              if (element.id === this.data.programacionData.evaluacionId) {
                this.f.evaluacion.setValue(element);
              }
            });
          }
        }
      });
  }

  private getEvaluadoresData() {
    this.evaluacionConocimientosService
      .ObtenerAplicacionPorAbreviatura('NUETAL')
      .subscribe((res) => {
        this.obtenerRolesPorAplicacion(res.aplicacion, 'Evaluador');
      });
  }

  private obtenerRolesPorAplicacion(aplicacionId: number, nombreRol: string) {
    this.evaluacionConocimientosService
      .ObtenerRolesPorAplicacion(aplicacionId, nombreRol)
      .subscribe((res) => {
        this.getEvaluadores(res.rolId);
      });
  }

  private getEvaluadores(idRol) {
    this.evaluacionConocimientosService
      .getAllPersonasCuentaEntidad(idRol)
      .subscribe((res) => {
        if (!this.authenticationRepository.isSuperAdminEntidad()) {
          this.autoEvaluadores = res;
          console.log("this.autoEvaluadores:",this.autoEvaluadores)
          if (this.isEditar) {
            if (this.autoEvaluadores != null && this.autoEvaluadores.length > 0) {
              this.autoEvaluadores.forEach((element) => {
                if (
                  element.personaId === this.data.programacionData.evaluadorId
                ) {
                  this.f.evaluador.setValue(element);
                }
              });
            }
          }
        } else {
          let nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
          let evaluadorDefaul = [{
            "cuentaEntidadId": null,
            "entidadId": this.authenticationRepository.getCurrentUserValue.entidadId,
            "rolId": this.authenticationRepository.getCurrentUserValue.rolId,
            "usuarioId": 237,
            "personaId": this.authenticationRepository.getCurrentUserValue
            .personaId,
            "nombreCompleto": this.authenticationRepository.getCurrentUserValue.nombreCompleto,
            "rolNombre": nombreRol
        }]

        this.autoEvaluadores = evaluadorDefaul;

        if (this.isEditar) {
          if (this.autoEvaluadores != null && this.autoEvaluadores.length > 0) {
            this.autoEvaluadores.forEach((element) => {
              if (
                element.personaId === this.data.programacionData.evaluadorId
              ) {
                this.f.evaluador.setValue(element);
              }
            });
          }
        }
          
        }
        
        console.log("this.autoEvaluadores:",this.autoEvaluadores)

      });
  }

  private getSede() {
    this.evaluacionConocimientosService.listarSedes().subscribe((res) => {
      this.cmbSede = res;
    });
  }

  private getModalidadesEvaluacion() {
    this.evaluacionConocimientosService
      .listarModalidadEvaluacion()
      .subscribe((res) => {
        this.cmbModalidad = res;
        if (this.isEditar) {
          this.validarTipoModalidadVirtual(
            this.cmbModalidad,
            this.data.programacionData.modalidadId
          );
        }
      });
  }

  private getDepartamentosData() {
    this.parametrosRepository.getDepartamento().subscribe((value) => {
      this.departamentos = value;
    });
  }

  changeAutoCompleteEvaluacion() {}

  changeAutoCompleteEvaluador() {}

  changeModalidad() {
    this.validarTipoModalidadVirtual(
      this.cmbModalidad,
      this.f.selModalidad.value
    );
  }

  cleanNuevaSedeInfo() {
    this.f.selSede.setValue(null);
    this.cleanInputSedeInfo();
  }

  cleanInputSedeInfo() {
    this.f.nombreSede.setValue(null);
    this.f.lugarEvaluacion.setValue(null);
    this.f.referencias.setValue(null);
    this.f.deptSelectValue.setValue(null);
    this.idDpto = null;
    this.cleanProvincias();
    this.cleanDistritos();
  }

  private validarTipoModalidadVirtual(cmbModalidad, id) {
    let codeDuracion = '';
    cmbModalidad.forEach((element) => {
      if (element.idMaeDuracion === id) {
        codeDuracion = element.descrDuracion;
      }
    });

    if (codeDuracion === 'VIRTUAL') {
      this.isOtroSelected = false;
      this.isVirtual = true;
      this.cleanNuevaSedeInfo();
    } else {
      this.isVirtual = false;
    }
  }

  changeSede() {
    this.cleanInputSedeInfo();
    if (this.f.selSede.value === 0) {
      this.isOtroSelected = true;
    } else {
      this.isOtroSelected = false;
      this.cmbSede.forEach((element) => {
        if (element.sedeId === this.f.selSede.value) {
          this.f.nombreSede.setValue(element.nombreSede);
          this.f.referencias.setValue(element.referenciaLugar);
        }
      });
    }
  }

  private validarCampos() {
    let isValid = true;

    if (this.f.cantidad.value <= 0) {
      isValid = false;
      this.toast.showToast('El campo Cantidad debe ser mayor a 0', 'danger');
      this.f.cantidad.setValue(null);
    }

    // valida nombreSede
    if (this.isOtroSelected) {
      if (this.f.nombreSede.value === '' || this.f.nombreSede.value === null) {
        isValid = false;
        this.toast.showToast(
          'Debe de ingresar el nuevo nombre de la sede',
          'danger'
        );
      }
    }

    // valida cantidad
    let maxValue = 0;
    if (this.isEditar) {
      maxValue =
        this.cantidadPostulantes -
        (this.totalPostulantesConvocados - this.data.programacionData.cantidad);
    } else {
      maxValue = this.cantidadPostulantes - this.totalPostulantesConvocados;
    }

    if (this.f.cantidad.value > maxValue) {
      this.toast.showToast(
        'Sólo puede ingresar como máximo ' + maxValue + ' vacantes',
        'danger'
      );
      this.f.cantidad.setValue(null);
      isValid = false;
    }

    // validar fechas
    let fecIni = moment(this.f.fechaInicio.value, 'DD/MM/YYYY HH:mm:ss');
    let fecFin = moment(this.f.fechaFin.value, 'DD/MM/YYYY HH:mm:ss');

    if (fecIni >= fecFin) {
      this.toast.showToast(
        'La fecha de inicio debe ser menor a la fecha de fin ',
        'danger'
      );
      this.f.fechaInicio.setValue(null);
      this.f.fechaFin.setValue(null);
      isValid = false;
    }

    return isValid;
  }

  onClickGuardar() {
    let isValid = this.validarCampos();
    console.log("this.f.evaluador:",this.f.evaluador)
   
    if (isValid) {
      if (this.registerForm.valid) {
        let ubigeoId = this.idDpto + ',' + this.idProv + ',' + this.idDistr;
        this.evaluacionConocimientosService
          .registrarProgramacion(
            this.f.evaluador.value.personaId,
            this.f.selModalidad.value,
            this.idConvocatoria,
            this.f.cantidad.value,
            this.nombreGrupo,
            this.f.fechaInicio.value,
            this.f.fechaFin.value,
            this.f.linkMeet.value,
            this.f.evaluacion.value.id,
            this.f.selSede.value,
            this.f.indicaciones.value,
            this.perfilId,
            this.tipoExamen,
            this.programacionId,
            this.f.nombreSede.value,
            ubigeoId,
            this.f.lugarEvaluacion.value,
            this.f.referencias.value
          )
          .subscribe((res) => {
            if (res.status.success) {
              this.toast.showToast(res.payload.mensaje, 'success', 'Atención');
              this.onNoClick(true);
            } else {
              this.toast.showToast(
                res.status.error.messages[0],
                'danger',
                'Atención'
              );
            }
          });
      } else {
        this.toast.showToast(
          'Debe completar los campos obligatorios',
          'danger'
        );
      }
    }
  }

  cambioDept(idDept: number) {
    this.cleanProvincias();
    this.cleanDistritos();
    this.parametrosRepository.getProvincias(idDept).subscribe((value) => {
      this.provincias = value;
    });
    this.idDpto = idDept;
  }

  cambioProv(idProv: number) {
    this.cleanDistritos();
    this.parametrosRepository.getDistritos(idProv).subscribe((value) => {
      this.distritos = value;
    });
    this.idProv = idProv;
  }

  cambioDistr(idDistr: number) {
    this.idDistr = idDistr;
  }

  cleanDistritos() {
    this.distritos = [];
    this.idDistr = null;
    this.f.distSelectValue.setValue(null);
  }

  cleanProvincias() {
    this.provincias = [];
    this.idProv = null;
    this.f.provSelectValue.setValue(null);
  }

  resetDept() {
    this.f.deptSelectValue.setValue(null);
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }

  closePicker() {
    this.picker.cancel();
  }
}
