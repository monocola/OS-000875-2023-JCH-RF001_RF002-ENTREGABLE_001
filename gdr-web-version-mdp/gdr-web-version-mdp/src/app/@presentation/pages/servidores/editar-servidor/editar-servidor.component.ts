import { DatePipe } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { ActivatedRoute } from '@angular/router';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { PuestoUoServidorCivil } from 'src/app/@data/model/puesto';
import { DatosPersonalesServidorCivil } from 'src/app/@data/model/servidores-civiles';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EditarPuestoComponent } from '../editar-puesto/editar-puesto.component';

@Component({
  selector: 'serv-talento-editar-servidor',
  templateUrl: './editar-servidor.component.html',
  styleUrls: ['./editar-servidor.component.scss']
})
export class EditarServidorComponent implements OnInit {
  registerForm: FormGroup;
  registerFormPuesto: FormGroup;
  generos: MaestraParametro[] = [];
  tiposDocumento: MaestraParametro[] = [];
  unidadesOrganicas: UnidadOrganicaCombo[] = [];
  sindicatos: MaestraParametro[] = [];
  tiposAsignacion: MaestraParametro[] = [];

  listaEncargados: any[] = [];
  listaPuestos: any[] = [];
  listaPuestosFiltrados: any[] = [];
  listaPuestosTablas: PuestoUoServidorCivil[] = [];
  columnasTablaPuestos: TableColumn[];

  datos: DatosPersonalesServidorCivil = {};

  idServidor: number;
  uoId: number;
  TIPO_ASIGNACION_ENCARGADO: number = 2;

  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private toastService: ToastService,
    private datePipe: DatePipe,
    private maeParametroRepository: MaestraParametroRepository,
    private servidoresRepository: ServidoresRepository,
    private UnidadOrganicaRepository: UnidadOrganicaRepository,
    private route: ActivatedRoute
    ) { }

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();

    this.route.queryParams
      .subscribe(params => {
        this.idServidor = Number(params.id);
        this.uoId = Number(params.uoId);
        this.cargarValores();
      }
    );
  }

  get f() { return this.registerForm.controls; }
  get fp() { return this.registerFormPuesto.controls; }

  initializeForm() {
    this.registerForm = this.fb.group({
      apellidoPaterno: [{ value: '', disabled: true }],
      apellidoMaterno: [{ value: '', disabled: true }],
      nombres: [{ value: '', disabled: true }],
      genero: [{ value: '', disabled: true }],
      fechaNacimiento: [{ value: '', disabled: true }],
      tipoDocumentoIdentidad: [{ value: '', disabled: true }],
      numeroDocumentoIdentidad: [{ value: '', disabled: true }],
      correo: ['', [Validators.required, Validators.email]],
      sindicato: ['', [Validators.required]]
    });

    this.registerFormPuesto = this.fb.group({
      organo: ['', [Validators.required]],
      puesto: ['', [Validators.required]],
      fechaInicio: ['', [Validators.required]],
      tipoAsignacion: ['', [Validators.required]],
      personaAsignada: ''
    });

    setTimeout(() => {
      const input = document.getElementById('fechaNacimiento');


      input.setAttribute('maxlength', '10');


      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };


      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };

      const fechaInicio = document.getElementById('fechaInicio');


      fechaInicio.setAttribute('maxlength', '10');


      fechaInicio.onkeydown = (e: any) => {
        return this.isNumeric(fechaInicio, e.keyCode);
      };


      fechaInicio.onkeyup = (e: any) => {
        this.validateDateFormat(fechaInicio, e.keyCode);
      };
    }, 250);
  }

  isShift: boolean = false;
  seperator: string = '/';
  isNumeric(input: any, keyCode: any) {
    console.log(keyCode);

    if (keyCode === 16) {
      this.isShift = true;
    }

    if (
      ((keyCode >= 48 && keyCode <= 57) ||
        keyCode === 8 ||
        keyCode === 46 ||
        keyCode === 37 ||
        keyCode === 39 ||
        (keyCode >= 96 && keyCode <= 105)) &&
      this.isShift === false
    ) {
      if (
        (input.value.length === 2 || input.value.length === 5) &&
        keyCode !== 8 && keyCode !== 46
      ) {
        input.value += this.seperator;
      }

      return true;
    } else {
      return false;
    }
  }

  validateDateFormat(input, keyCode) {
    let dateString = input.value;
    if (keyCode === 16) {
      this.isShift = false;
    }
    let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    } else {
      // Es invalido
    }
  }

  initializeColumns() {
    this.columnasTablaPuestos = [
      { name: 'Nro', dataKey: 'correlativo', position: 'left', isSortable: true },
      { name: 'Puesto', dataKey: 'puesto', position: 'left', isSortable: true },
      { name: 'Órgano / UO / Sub UO', dataKey: 'siglaUO', position: 'left', isSortable: true },
      { name: 'Estado', dataKey: 'estado', position: 'left', isSortable: true },
      { name: 'Fecha Inicio', dataKey: 'fechaInicioTexto', position: 'left', isSortable: true },
      { name: 'Fecha Cese', dataKey: 'fechaCeseTexto', position: 'left', isSortable: true },
    ];
  }

  cargarValores() {
    const getGeneros = this.maeParametroRepository.getMaestraParametro('PER_SEXO');
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo();
    const getSindicatos = this.maeParametroRepository.getMaestraParametro('PER_SINDICATO');
    const getTipoAsignacion = this.maeParametroRepository.getMaestraParametro('TIPO_ASIGNACION_SERVIDOR_CIVIL');
    const getPuestos = this.UnidadOrganicaRepository.getPuestos();
    const getDatos = this.servidoresRepository.getDatosPersonales(this.idServidor);

    forkJoin([getGeneros, getTipoDocumento, getUndOrganicaCbo, getSindicatos, getTipoAsignacion,
      getPuestos, getDatos])
      .subscribe(
        (results) => {
          this.generos = results[0];
          this.tiposDocumento = results[1];
          this.unidadesOrganicas = results[2];
          this.sindicatos = results[3];
          this.tiposAsignacion = results[4];
          this.listaPuestosFiltrados = this.listaPuestos = results[5];
          this.cargarDatosPersonales(results[6]);
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
  }

  cargarDatosPersonales(datos: DatosPersonalesServidorCivil) {
    this.datos = datos;
    setTimeout(() => {
      this.registerForm.get('apellidoPaterno').setValue(this.datos.apellidoPaterno);
      this.registerForm.get('apellidoMaterno').setValue(this.datos.apellidoMaterno);
      this.registerForm.get('nombres').setValue(this.datos.mombres);
      this.registerForm.get('genero').patchValue(this.datos.genero);
      this.registerForm.get('fechaNacimiento').setValue(this.datos.fechaNacimiento);
      this.registerForm.get('correo').setValue(this.datos.correoInstitucional);
      this.registerForm.get('tipoDocumentoIdentidad').patchValue(this.datos.tipoDocumento);
      this.registerForm.get('numeroDocumentoIdentidad').setValue(this.datos.numeroDocumento);
      this.registerForm.get('sindicato').patchValue(this.datos.sindicato);

      this.registerFormPuesto.get('organo').patchValue(this.uoId);
      this.actualizarTablaPuestos(this.uoId);
    });
  }

  onChangePuesto($event: any) {
    let valor = $event.target.value.trim().toLowerCase();
    this.listaPuestosFiltrados = this.listaPuestos.filter(x => x.descripcion.toLowerCase().includes(valor));
  }

  onSelectionChangePuesto(idPuesto: number) {
    let seleccionado = this.listaPuestos.find(x => x.id === idPuesto);
    if (seleccionado) {
      this.registerFormPuesto.get('puesto').setValue(seleccionado.descripcion);
    }
  }

  actualizar() {
    this.registerForm.markAllAsTouched();
    if (this.registerForm.valid) {
      this.datos.correoInstitucional = this.registerForm.get('correo').value;
      this.datos.sindicato = this.registerForm.get('sindicato').value;
      this.servidoresRepository.actualizarServidor(this.datos)
        .subscribe(x => {
          this.toastService.showToast(
            'Se realizó la actualización exitosamente',
            'success'
          );
        },
        (err) => this.toastService.showToast(err, 'danger'));
    }
  }

  agregarPuesto() {
    this.registerFormPuesto.markAllAsTouched();
    if (this.registerFormPuesto.valid) {
      const uoId: number = this.registerFormPuesto.get('organo').value;
      const datos: PuestoUoServidorCivil = {
        uoId: uoId,
        personaId: this.idServidor,
        puesto: this.registerFormPuesto.get('puesto').value,
        fechaInicio: this.registerFormPuesto.get('fechaInicio').value,
        tipoAsignacion: this.registerFormPuesto.get('tipoAsignacion').value,
        personaIdAsignada: this.registerFormPuesto.get('personaAsignada').value
      };

      this.servidoresRepository.agregarPuesto(datos)
        .subscribe(x => {
          this.actualizarTablaPuestos(uoId);
          this.registerFormPuesto.reset();
          this.onSelectionChangeTipoAsignacion(null);
          this.registerFormPuesto.get('organo').setValue(uoId);

          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );
        },
        (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  editPuesto(item: PuestoUoServidorCivil) {
    this.dialog.open(EditarPuestoComponent, {
      disableClose: true,
      data: {
        ...item,
        personaId: this.idServidor
      }
    })
      .afterClosed()
      .subscribe(procesado => {
        if (procesado) {
          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );
          const uoId: number = this.registerFormPuesto.get('organo').value;
          this.actualizarTablaPuestos(uoId);
        }
      });
  }

  removePuesto(item: any) {
    // this.dataToDelete = item;
    // this.dialog.open(ModalEliminarComponent);
  }

  actualizarEncargados() {
    this.registerFormPuesto.get('personaAsignada').setValue('');

    const idUO = this.registerFormPuesto.get('organo').value;
    const tipoAsignacion = this.registerFormPuesto.get('tipoAsignacion').value;
    if (idUO && tipoAsignacion === this.TIPO_ASIGNACION_ENCARGADO) {
      this.servidoresRepository.listarPersonasParaPuesto(idUO, tipoAsignacion)
        .subscribe(x => {
          this.listaEncargados = x;
        });
    } else {
      this.listaEncargados = [];
    }
  }

  actualizarTablaPuestos(uoId: number) {
    this.servidoresRepository.getHisotrialPuestos(this.idServidor, uoId)
      .subscribe(x => {
        this.listaPuestosTablas = x.map(p => {
          return {
            ...p,
            fechaInicioTexto: this.datePipe.transform(p.fechaInicio, 'dd/MM/yyyy'),
            fechaCeseTexto: this.datePipe.transform(p.fechaCese, 'dd/MM/yyyy')
          };
        });
      });
  }

  onSelectionChangeUO(uoId?: number) {
    this.actualizarEncargados();
    this.actualizarTablaPuestos(uoId);
  }

  onSelectionChangeTipoAsignacion(tipoAsignacion: number) {
    this.actualizarEncargados();
    const control = this.registerFormPuesto.get('personaAsignada');
    if (tipoAsignacion === this.TIPO_ASIGNACION_ENCARGADO) {
      control.setValidators(Validators.required);
    } else {
      control.clearValidators();
    }
    control.updateValueAndValidity();
  }

  getDataExport() {
    return null;
  }
}
