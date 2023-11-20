import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { ToastService } from '../../@common-components/toast';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { forkJoin } from 'rxjs';
import { ParameterRepository } from '../../../@domain/repository/parameter.repository';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { AuthenticationRepository } from '../../../@domain/repository/authentication.repository';
import { SolicitudRepository } from '../../../@domain/repository/solicitud.repository';
import { MatDialog } from '@angular/material/dialog';
import { ServidoresRepository } from '../../../@domain/repository/servidores.repository';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { SolicitudExternaService } from '../../../@data/services/solicitud-externa.service';
import { HttpEventType, HttpResponse } from '@angular/common/http';
import { getBase64 } from '../../../utils/converterFile';
import { Utils } from '../../../utils/utils';
import { ActivatedRoute, Router } from '@angular/router';
import { NbStepperComponent } from '@nebular/theme';
import { ModalQuestionComponent } from '../../pages/gestores-orh/modal-question/modal-question.component';
import { SolicitudExternaRepository } from 'src/app/@domain/repository/solicitud.externa.repository';
import { SolicitudExterna } from 'src/app/@data/model/SolicitudExterna';
import * as utf8 from 'crypto-js/enc-utf8';
import * as AES from 'crypto-js/aes';
import * as CryptoJS from 'crypto-js';
import { EntidadService } from 'src/app/@data/services/entidad.service';
import { Const } from 'src/app/@data/services/const';


@Component({
  selector: 'gme-web-solicitud-editar',
  templateUrl: './solicitud-editar.component.html',
  styleUrls: ['./solicitud-editar.component.scss'],
})
export class SolicitudEditarComponent implements OnInit {
  @ViewChild('stepper') stepper: NbStepperComponent;
  @ViewChild('myRuc') myRuc: ElementRef;
  @ViewChild('myPantalla') myPantalla: ElementRef;

  frm: FormGroup = null;
  cboNivel: MaestraParametro[] = [];
  cboSector: MaestraParametro[] = [];
  cboTipoEntPub: MaestraParametro[] = [];
  cboTipoDocumento: MaestraParametro[] = [];
  filesSize: File[] = [];

  token: any = null;
  conteoupload: number = 0;
  firstForm: FormGroup = null;
  secondForm: FormGroup = null;
  searchForm: FormGroup = null;
  estaRegistrado: boolean = false;
  cantidadSol: number = 0;
  numeroDocumentoType: string = 'integer';
  selectedStepIndex: number = 0;
  hidden1: boolean = false;
  sizeFile: [];
  solicitudExtId: number;
  solicitudExtIdStr: String;
  solicitud: SolicitudExterna = null;
  logoProfile;
  nomArchivo = '';
  imgData64 = '';
  imgFile: File = null;
  flagUpdatePhoto = 0;
  numeroDocumentoMaxlength: number = 8;
  foto: boolean = false;
  tipoDoc: boolean = false;

  constructor(
    private route: Router,
    private routeActive: ActivatedRoute,
    private toastService: ToastService,
    private fb: FormBuilder,
    private authenticationRepository: AuthenticationRepository,
    private parameterRepository: ParameterRepository,
    private solicitudRepository: SolicitudRepository,
    private servidoresRepository: ServidoresRepository,
    private maestraParametroRepository: MaestraParametroRepository,
    private solicitudExternaService: SolicitudExternaService,
    private entidad: EntidadService,

    private dialog: MatDialog
  ) {
    this.routeActive.queryParams
      .subscribe(params => {
        const idParams = params.id;
        this.solicitudExtIdStr = idParams.replaceAll(' ', '+');
        console.log(this.solicitudExtIdStr);
        this.decrypt();
      }
      );
    this.initializeForm();

  }

  isShift: boolean = false;
  seperator: string = '/';
  isNumeric(input: any, keyCode: any) {
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
        keyCode !== 8 &&
        keyCode !== 46
      ) {
        input.value += this.seperator;
      }

      return true;
    } else {
      return false;
    }
  }

  decrypt() {

    var base64EncodedKeyFromJava = 'QUJDREVGR0hJSktMTU5PUA==';
    var keyForCryptoJS = CryptoJS.enc.Base64.parse(base64EncodedKeyFromJava);

    var encryptString = this.solicitudExtIdStr;
    var decodeBase64 = CryptoJS.enc.Base64.parse(encryptString)

    var decrypted = CryptoJS.AES.decrypt({
      ciphertext: decodeBase64
    }, keyForCryptoJS, {
      mode: CryptoJS.mode.ECB,
      padding: CryptoJS.pad.Pkcs7
    });

    var decryptedText = decrypted.toString(CryptoJS.enc.Utf8);

    this.solicitudExtId = decryptedText;

  }


  validateDateFormat(input, keyCode) {
    let dateString = input.value;
    if (keyCode === 16) {
      this.isShift = false;
    }
    //let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    //if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    //} else {
      // Es invalido
    //}
  }

  
  ngOnInit(): void {
    this.getToken();
  }

  initializeForm() {
    this.frm = this.fb.group({
      tipoDocumento: new FormControl(null, [Validators.required]),
      numeroDocumento: new FormControl(null, [
        Validators.required,
        Validators.maxLength(15),
        Validators.minLength(8),
      ]),
      apellidoPaterno: new FormControl(null, [
        Validators.required,
        Validators.maxLength(250),
      ]),
      apellidoMaterno: new FormControl(null, [
        Validators.required,
        Validators.maxLength(250),
      ]),
      nombres: new FormControl(null, [
        Validators.required,
        Validators.maxLength(250),
      ]),
      fechaNacimiento: new FormControl(null, Validators.required),
      telefonoFijo: new FormControl(null, [
        Validators.required,
        //Validators.minLength(1),
        Validators.maxLength(20),
      ]),
      anexo: new FormControl(null),
      celular: new FormControl(null, [Validators.required,
      //Validators.minLength(1),
      Validators.maxLength(20)
      ]),
      correoElectronico: new FormControl(null, [
        Validators.required,
        Validators.email,
      ]),
      correoElectronicoGestor: new FormControl(null, [Validators.email]),
      archivo: new FormControl(null),
    });

    this.firstForm = this.fb.group({
      base64Image: new FormControl(null),
      abreviaturaEntidad: new FormControl(null, [
        Validators.required,
        Validators.maxLength(10),
      ]),
      razonSocial: new FormControl(null),
      razonSocialSearch: new FormControl(null, [
        // Validators.required,
        // Validators.maxLength(10),
      ]),
      nombreEntidad: new FormControl(null),
      sindicato: new FormControl(null),
      nivelGobiernoId: new FormControl(null),
      sectorId: new FormControl(null),
      tipoEntidadId: new FormControl(null),
    });

    this.searchForm = this.fb.group({
      rucEntidad: new FormControl(null),
      rucEntidadSearch: new FormControl(null, [
        Validators.required,
        Validators.minLength(11),
        Validators.maxLength(11),
      ]),
    });
  }

  changeType(event){
    this.frm.patchValue({
      numeroDocumento: null,
    });
    if (event == 4) {
      this.numeroDocumentoMaxlength = 12;
      this.tipoDoc = true;
      this.frm.get('numeroDocumento').setValidators([Validators.required, Validators.minLength(9)]);
      this.frm.get('numeroDocumento').updateValueAndValidity();
    } else {
      this.numeroDocumentoMaxlength = 8;
      this.tipoDoc = false;
      this.frm.get('numeroDocumento').setValidators([Validators.required, Validators.minLength(8)]);
      this.frm.get('numeroDocumento').updateValueAndValidity();
    }
  }

  cargarInfo() {
    if (this.token) {
      //this.solicitudExtId = 117
      const getSolicitud = this.solicitudRepository.getSolicitudExtId(
        this.solicitudExtId
      );
      forkJoin([getSolicitud]).subscribe(
        (results) => {
          this.solicitud = results[0];
          this.llenarInfoStep1();
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
    }
  }

  llenarInfoStep1() {
    if (this.solicitud.urlLogoEntidad!=null) {
      this.logoProfile = Const.API_FILE_SERVER  + this.solicitud.urlLogoEntidad;
      this.foto = true
    }
     
    this.searchForm.controls['rucEntidad'].setValue(this.solicitud.rucEntidad);
    this.searchForm.controls['rucEntidad'].patchValue(this.solicitud.rucEntidad);

    this.searchForm.controls['rucEntidadSearch'].setValue(this.solicitud.rucEntidad);
    this.searchForm.controls['rucEntidadSearch'].patchValue(this.solicitud.rucEntidad);

    this.frm.controls['numeroDocumento'].setValue(this.solicitud.numeroDocumento);
    this.frm.controls['numeroDocumento'].patchValue(this.solicitud.numeroDocumento);
    this.firstForm.controls['abreviaturaEntidad'].setValue(this.solicitud.abreviatura);
    this.firstForm.controls['abreviaturaEntidad'].patchValue(this.solicitud.abreviatura);

    this.firstForm.controls['razonSocial'].setValue(this.solicitud.razonSocial);
    this.firstForm.controls['razonSocial'].patchValue(this.solicitud.razonSocial);

    this.firstForm.controls['nombreEntidad'].setValue(this.solicitud.nombreEntidad);
    this.firstForm.controls['nombreEntidad'].patchValue(this.solicitud.nombreEntidad);

    //falta
    this.firstForm.controls['sindicato'].setValue(this.solicitud.sindicato);
    this.firstForm.controls['sindicato'].patchValue(this.solicitud.sindicato);

    this.firstForm.controls['nivelGobiernoId'].setValue(this.solicitud.nivelGobiernoId);
    this.firstForm.controls['nivelGobiernoId'].patchValue(this.solicitud.nivelGobiernoId);

    console.log("sector id", this.solicitud.sectorId)
    this.firstForm.controls['sectorId'].setValue(this.solicitud.sectorId);
    this.firstForm.controls['sectorId'].patchValue(this.solicitud.sectorId);

    this.firstForm.controls['tipoEntidadId'].setValue(this.solicitud.tipoEntidadId);
    this.firstForm.controls['tipoEntidadId'].patchValue(this.solicitud.tipoEntidadId);


  }

  llenarInfoStep2() {

    this.frm.controls['tipoDocumento'].setValue(this.solicitud.tipoDocumento);
    this.frm.controls['tipoDocumento'].patchValue(this.solicitud.tipoDocumento);
    if (this.frm.value.tipoDocumento == 4) {
      this.numeroDocumentoMaxlength = 12;
      this.frm.get('numeroDocumento').setValidators([Validators.required, Validators.minLength(9)]);
      this.frm.get('numeroDocumento').updateValueAndValidity();
    } 

    this.frm.controls['numeroDocumento'].setValue(this.solicitud.numeroDocumento);
    this.frm.controls['numeroDocumento'].patchValue(this.solicitud.numeroDocumento);

    this.frm.controls['apellidoPaterno'].setValue(this.solicitud.apellidoPaterno);
    this.frm.controls['apellidoPaterno'].patchValue(this.solicitud.apellidoPaterno);

    this.frm.controls['apellidoMaterno'].setValue(this.solicitud.apellidoMaterno);
    this.frm.controls['apellidoMaterno'].patchValue(this.solicitud.apellidoMaterno);

    this.frm.controls['nombres'].setValue(this.solicitud.nombres);
    this.frm.controls['nombres'].patchValue(this.solicitud.nombres);

    var fechaArray = "1999-01-01";
    fechaArray = this.solicitud.fechaNacimiento.split("/");
    var anno = fechaArray[2];
    var mes = fechaArray[1];
    var dia = fechaArray[0];

    var dateString = anno + "-" + mes + "-" + dia + " " + "00:00:00";

    this.frm.controls['fechaNacimiento'].setValue(new Date(dateString));
    this.frm.controls['fechaNacimiento'].patchValue(new Date(dateString));

    this.frm.controls['anexo'].setValue(this.solicitud.anexo);
    this.frm.controls['anexo'].patchValue(this.solicitud.anexo);

    this.frm.controls['celular'].setValue(this.solicitud.celular);
    this.frm.controls['celular'].patchValue(this.solicitud.celular);

    this.frm.controls['telefonoFijo'].setValue(this.solicitud.telefonoFijo);
    this.frm.controls['telefonoFijo'].patchValue(this.solicitud.telefonoFijo);

    this.frm.controls['correoElectronico'].setValue(this.solicitud.correoElectronico);
    this.frm.controls['correoElectronico'].patchValue(this.solicitud.correoElectronico);

    this.frm.controls['correoElectronicoGestor'].setValue(this.solicitud.correoGestorGdr);
    this.frm.controls['correoElectronicoGestor'].patchValue(this.solicitud.correoGestorGdr);


  }

  get f() {
    return this.frm.controls;
  }

  get ff() {
    return this.firstForm.controls;
  }

  get fff() {
    return this.searchForm.controls;
  }

  onFirstSubmit() {
    this.firstForm.controls.abreviaturaEntidad.markAsDirty();
    this.firstForm.controls.razonSocial.markAsDirty();
    this.firstForm.controls.razonSocialSearch.markAsDirty();
  }

  validacion() {
    this.onFirstSubmit();
    if (this.firstForm.status !== 'VALID') {
      return false;
    }
    /*if (this.solicitud.abreviatura != this.firstForm.get('abreviaturaEntidad').value) {
      this.entidad.searchSigla(this.firstForm.get('abreviaturaEntidad').value).subscribe(
        (results) => {
          if (results.length > 0) {
            this.toastService.showToast(
              'El nombre abreviado de la entidad ingresada ya existe',
              'danger'
            );
            return false;
          } else {
            this.llenarInfoStep2();
            this.stepper.selectedIndex = 1;
          }
        }
      );
    } else {*/
      this.llenarInfoStep2();
      this.stepper.selectedIndex = 1;
    //}
    

  }

  resetStep2 = () => {
    this.frm.reset();
  }

  refresh(): void {
    window.location.reload();
  }

  onSecondSubmit() {
    this.secondForm.markAsDirty();
  }

  fileLogoEvent(fileInput) {
    console.log("FOTO1111", fileInput)
    if (fileInput.target.files[0].size / (1024 * 1024) > 0.5) {
      this.toastService.showToast(
        'El archivo excede el tamaño de 500Kb',
        'danger'
      );
    } else {
      const extension =
        fileInput.target.files[0].name.split('.')[
        fileInput.target.files[0].name.split('.').length - 1
        ];
      const extensionesPermitidas = [
        'jpg',
        'JPG',
        'png',
        'PNG',
        'JPEG',
        'jpeg',
      ];
      if (extensionesPermitidas.includes(extension)) {
        this.imgFile = fileInput.target.files[0] as File;
        this.foto = true;
        this.nomArchivo = this.imgFile.name;
        getBase64(this.imgFile).then((data: string) => {
          this.flagUpdatePhoto = 0;
          this.logoProfile = null;
          this.imgData64 = data;
          setTimeout(() => {
            let imageRef: any = document.getElementById("logoProfile");
            imageRef.src = this.imgData64;
          }, 0);
        });
        console.log("FOTO333", this.imgData64)
      } else {
        this.imgFile = null;
        this.foto = false;
        this.toastService.showToast(
          'Solo están permitidos los archivos jpg, png, jpeg',
          'danger'
        );
      }
    }
  }

  clearImage() {
    this.logoProfile = null;
    this.imgData64 = '';
    this.imgFile = null;
    this.foto = false
  }

  validarCamposObligatorios() {
    this.frm.controls.celular.markAsDirty();
    this.frm.controls.telefonoFijo.markAsDirty();

    if ((this.frm.controls.telefonoFijo.value === undefined ||
      this.frm.controls.telefonoFijo.value === null ||
      this.frm.controls.telefonoFijo.value === '') &&
      (this.frm.controls.celular.value === undefined ||
        this.frm.controls.celular.value === null ||
        this.frm.controls.celular.value === '')) {
      this.camposObligatorios();
      document.getElementById('myPantalla').click();
      return false;
    }

    if (
      this.frm.controls.telefonoFijo.value !== undefined &&
      this.frm.controls.telefonoFijo.value !== null &&
      this.frm.controls.telefonoFijo.value !== ''
    ) {
      this.frm.get('celular').clearValidators();
      this.frm.get('celular').updateValueAndValidity();
      document.getElementById('myPantalla').click();

    }

    if (
      this.frm.controls.celular.value !== undefined &&
      this.frm.controls.celular.value !== null &&
      this.frm.controls.celular.value !== ''
    ) {
      this.frm.get('telefonoFijo').clearValidators();
      this.frm.get('telefonoFijo').updateValueAndValidity();
      document.getElementById('myPantalla').click();

    }
  }

  validarCamposObligatoriosGeneral() {

    if (
      (this.frm.controls.celular.value == undefined ||
        this.frm.controls.celular.value == null ||
        this.frm.controls.celular.value == '') &&
      (this.frm.controls.telefonoFijo.value !== undefined ||
        this.frm.controls.telefonoFijo.value !== null ||
        this.frm.controls.telefonoFijo.value !== '')
    ) {
      this.frm.controls.celular.markAsDirty();
      this.frm.controls.telefonoFijo.markAsDirty();
       
    } else {
      this.frm.get('celular').setValidators(null); 
      this.frm.get('celular').setErrors(null); 
      this.frm.get('telefonoFijo').setValidators(null); 
      this.frm.get('telefonoFijo').setErrors(null);
    }

    if (
      (this.frm.controls.celular.value != undefined ||
        this.frm.controls.celular.value != null ||
        this.frm.controls.celular.value != '') &&
      (this.frm.controls.telefonoFijo.value == undefined ||
        this.frm.controls.telefonoFijo.value == null ||
        this.frm.controls.telefonoFijo.value == '')
    ) {
      console.log("entre aaa")
      this.frm.controls.celular.markAsDirty();
      this.frm.controls.telefonoFijo.markAsDirty();
       
    } else {
      this.frm.get('celular').setValidators(null); 
this.frm.get('celular').setErrors(null); 
this.frm.get('telefonoFijo').setValidators(null); 
this.frm.get('telefonoFijo').setErrors(null);
    }

    this.frm.controls.tipoDocumento.markAsDirty();
    this.frm.controls.numeroDocumento.markAsDirty();
    this.frm.controls.apellidoPaterno.markAsDirty();
    this.frm.controls.apellidoMaterno.markAsDirty();
    this.frm.controls.nombres.markAsDirty();
    this.frm.controls.fechaNacimiento.markAsDirty();
    this.frm.controls.apellidoMaterno.markAsDirty();
    this.frm.controls.correoElectronico.markAsDirty();
    
  }


  validarCamposObligatoriosDown(event) {

    setTimeout(() =>
      this.validarCamposObligatorios(),
      10
    );
  }

  camposObligatorios() {
    this.frm.get('celular').setValidators([Validators.required, Validators.maxLength(20)]);
    this.frm.get('celular').updateValueAndValidity();
    this.frm.get('telefonoFijo').setValidators([Validators.required, Validators.maxLength(20)]);
    this.frm.get('telefonoFijo').updateValueAndValidity();
  }

  enviar() {
    this.validarCamposObligatoriosGeneral();

    if (
      this.firstForm.controls.razonSocial.value &&
      this.frm.valid &&
      this.file
    ) {
      const add = this.dialog.open(ModalQuestionComponent, {
        data: {
          title: '',
          bodyText:
            'Se enviará la solicitud a SERVIR <br> ¿Está realmente seguro de realizar la siguiente acción?',
          rutaImagen: './assets/images/question.png',
          textCancel: 'NO',
          textOk: 'SI',
        },
      });
      add.afterClosed().subscribe((res) => {
        if (res) {
          this.subirArchivoPdf();
        } else {
          this.toastService.showToast(
            'Cancelo el registro de solicitud',
            'danger',
            'Error'
          );
        }
      });
    } else {
      if (this.firstForm.controls.razonSocial.value.length === 0) {
        this.toastService.showToast(
          'Realice la busqueda del RUC',
          'warning',
          'Registro Solicitud'
        );
      } else if (!this.frm.valid) {
        this.toastService.showToast(
          'Ingrese los campos requeridos',
          'warning',
          'Registro Solicitud'
        );
      } else if (!this.file) {
        this.toastService.showToast(
          'Pendiente subir documento',
          'warning',
          'Registro Solicitud'
        );
      }
    }
  }

  getToken() {
    this.authenticationRepository.generatePublicToken().subscribe((item) => {
      this.token = item;
      this.loadCombox();
    });
  }

  file: File;
  archiveSelection(events) {

    this.filesSize.push(...events.addedFiles);

    if (this.filesSize.length === 1) {
      if (events.addedFiles[0].size <= 1101899) {
        if (events.addedFiles[0].type === 'application/pdf') {
          this.file = events.addedFiles[0];
        } else {
          this.toastService.showToast(
            'Subir un archivo con extensión PDF',
            'danger'
          );
        }
      } else {
        this.toastService.showToast(
          'Ingrese un archivo PDF menor a 1 MB',
          'danger'
        );
      }
    } else {
      this.toastService.showToast(
        'Solo esta permitido subir un archivo por registro',
        'danger'
      );
    }
    this.filesSize = [];
  }

  deleteFile() {
    this.file = null;
  }

  loadCombox() {
    const getNivel = this.parameterRepository.getNiveles();
    const getSector = this.parameterRepository.getSectores();
    const getTipoFilter = this.parameterRepository.getTipoEntidad();
    const getTipoDoc =
      this.maestraParametroRepository.getMaestraParametro('SOL_TIPO_DOCUMENTO');
    forkJoin([getNivel, getSector, getTipoFilter, getTipoDoc]).subscribe(
      (results) => {
        this.cboNivel = results[0];
        this.cboSector = results[1];
        this.cboTipoEntPub = results[2];
        this.cboTipoDocumento = results[3];
        console.log(this.cboNivel)
        this.cargarInfo();

      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  buscarRucPrueba() {
  }
  buscarRuc() {
    this.estaRegistrado = false;

    if (this.token) {
      this.solicitudRepository
        .getRuc(this.searchForm.value.rucEntidadSearch)
        .subscribe((item) => {
          this.searchForm.controls.rucEntidad.setValue(
            this.searchForm.value.rucEntidadSearch
          );

          if (
            item.payload.entidadId != null &&
            item.payload.validadoSunat !== null
          ) {
            this.estaRegistrado = true;
            this.seteaSolicitud(item);
          }

          if (
            (item.payload.validadoSunat == null &&
              item.payload.cantidadSol > 0) ||
            (item.payload.validadoSunat !== null &&
              item.payload.cantidadSol > 0)
          ) {
            /* se quita validacion a peticion de  QA */
            // this.toastService.showToast(
            //   'Usted tiene una solicitud de registro pendiente',
            //   'danger'
            // );
            this.seteaSolicitud(item);
            this.cantidadSol = 1;
          } else if (
            item.payload.validadoSunat === 'S' &&
            item.payload.cantidadSol === 0
          ) {
            this.firstForm.controls.razonSocialSearch.setValue(
              item.payload.razonSocial
            );
            this.firstForm.controls.razonSocial.setValue(
              item.payload.razonSocial
            );

            this.firstForm.controls.nombreEntidad.setValue(
              item.payload.nombreEntidad
            );

          } else if (
            item.payload.validadoSunat == null &&
            item.payload.entidadId != null
          ) {
            this.estaRegistrado = true;
            this.seteaSolicitud(item);
          } else if (
            item.payload.validadoSunat == null &&
            item.payload.cantidadSol === 0 &&
            item.payload.personaId == null
          ) {
            this.toastService.showToast('El RUC no existe', 'danger');
            this.limpiarEntidad();
          } else {
            this.toastService.showToast('El RUC no existe', 'danger');
            this.limpiarEntidad();
          }
        });
    }

  }

  limpiarEntidad() {
    this.firstForm.controls.razonSocial.setValue('');
    this.firstForm.controls.nombreEntidad.setValue('');
    this.searchForm.controls.rucEntidad.setValue('');
  }

  seteaSolicitud(item: any) {
    this.firstForm.controls.razonSocial.setValue(item.payload.razonSocial);
    this.firstForm.controls.razonSocialSearch.setValue(item.payload.razonSocial);
    this.firstForm.controls.nombreEntidad.setValue(item.payload.nombreEntidad);


    // this.firstForm.controls.razonSocial.disable();
    this.firstForm.controls.abreviaturaEntidad.setValue(
      item.payload.abreviatura
    );
    this.firstForm.controls.sindicato.setValue(item.payload.sindicato);
    this.firstForm.controls.nivelGobiernoId.setValue(item.payload.nivelGobId);
    this.firstForm.controls.sectorId.setValue(item.payload.sectorId);
    this.firstForm.controls.tipoEntidadId.setValue(item.payload.tipoEntidadId);
  }

  subirArchivoPdf() {
    if (this.file) {
      const bodyParam = {
        extensiones: 'EXTENSIONES_REG_SOL_ENT_EXTERN',
        ruc: this.searchForm.value.rucEntidad,
        ruta: 'alfresco.image.path.sol.ext',
      };
 
      this.solicitudExternaService
        .subirDocumentoSolicitudExterna(bodyParam, this.file)
        .subscribe((response: any) => {
          if (response.type === HttpEventType.UploadProgress) {
            this.conteoupload = Math.round(
              (100 * response.loaded) / response.total
            );
          } else if (response instanceof HttpResponse) {
            if (!response.body.status.success) {
              this.toastService.showToast(
                response.body.status.error.messages[0],
                'danger'
              );
            } else {
              this.registrarSolicitudExterna(response.body.payload.uuid);
            }
          }
        });
    }
  }

  registrarSolicitudExterna(uuid: any) {
    console.log("ENVIADO", this.imgData64)
    const niv = this.cboNivel.find(
      (niv) => niv.parametroId === this.frm.value.nivelGobiernoId
    );
    const sec = this.cboSector.find(
      (sec) => sec.parametroId === this.frm.value.sectorId
    );
    const tipoEntidad = this.cboTipoEntPub.find(
      (tipEnt) => tipEnt.parametroId === this.frm.value.tipoEntidadId
    );
    const tipoDoc = this.cboTipoDocumento.find(
      (tipDoc) => tipDoc.codigoNumero === this.frm.value.tipoDocumento
    );

    let fechaNacimiento = null;
    if (this.frm.value.fechaNacimiento) {
      fechaNacimiento = Utils.formatFechaDate(
        this.frm.value.fechaNacimiento,
        'DD/MM/YYYY'
      );
    }

    const bodyParam = {
      rucEntidad: this.searchForm.value.rucEntidad,
      razonSocial: this.firstForm.controls.razonSocial.value,
      nombreEntidad: this.firstForm.controls.nombreEntidad.value,
      abreviatura: this.firstForm.value.abreviaturaEntidad,
      sindicato: this.firstForm.value.sindicato,
      nivelGobiernoId: this.firstForm.value.nivelGobiernoId,
      nivelGobierno: niv != null ? niv.descripcion : null,
      sectorId: this.firstForm.value.sectorId,
      sector: sec != null ? sec.descripcion : null,
      tipoEntidadId: this.firstForm.value.tipoEntidadId,
      tipoEntidad: tipoEntidad != null ? tipoEntidad.descripcion : null,
      tipoDocumento: tipoDoc != null ? tipoDoc.codigoNumero : null,
      numeroDocumento: this.frm.value.numeroDocumento,
      apellidoPaterno: this.frm.value.apellidoPaterno,
      apellidoMaterno: this.frm.value.apellidoMaterno,
      nombres: this.frm.value.nombres,
      fechaNacimiento: fechaNacimiento,
      telefonoFijo: this.frm.value.telefonoFijo,
      anexo: this.frm.value.anexo,
      celular: this.frm.value.celular,
      correoElectronico: this.frm.value.correoElectronico,
      correoElectronicoGestor: this.frm.value.correoElectronicoGestor,
      estadoSolicitud: 1,
      uuId: uuid,
      base64Image: this.imgData64,
      solicitudExtId: this.solicitudExtId
    };

    this.solicitudExternaService
      .actualizarSolicitudExterna(bodyParam)
      .subscribe((response: any) => {
        if (response.status.success) {
          this.toastService.showToast(
            'Se registró la solicitud con Éxito',
            'success',
            'Atención'
          );
          this.authenticationRepository.logout();
        } else {
          if (response.status.error.messages.length > 0) {
            let respTexto = response.status.error.messages[0];
            let cad = respTexto.substring(
              respTexto.indexOf('[') + 2,
              respTexto.indexOf(']') - 1
            );
            this.toastService.showToast(cad, 'danger');
          } else {
            let respTexto = response.payload.mensajeRespuesta;
            let cad = respTexto.substring(
              respTexto.indexOf('[') + 2,
              respTexto.indexOf(']') - 1
            );
            this.toastService.showToast(cad, 'danger');
          }

        }
      });
  }

  regresar() {
    const add = this.dialog.open(ModalQuestionComponent, {
      data: {
        title: ' ',
        bodyText:
          'Al retroceder perderá todos los datos ingresados <br> ¿Está realmente seguro de realizar la siguiente acción?',
        rutaImagen: './assets/images/question.png',
        textCancel: 'NO',
        textOk: 'SI',
      },
    });
    add.afterClosed().subscribe((res) => {
      if (res) {
        this.authenticationRepository.logout();
      }
    });
  }

  numberOnly(event): boolean {
    console.log(event);
    const charCode = event.which ? event.which : event.keyCode;
    if (charCode > 31 && (charCode < 48 || charCode > 57)) {
      return false;
    }
    return true;
  }

  dateChange(event: any) {
    console.log(event);
  }

  stepChange(event) {
    console.log(event);

    const fechaN = document.getElementById('fechaN');
    if (fechaN) {
      fechaN.setAttribute('maxlength', '10');
      fechaN.onkeydown = (e: any) => {
        return this.isNumeric(fechaN, e.keyCode);
      };

      fechaN.onkeyup = (e: any) => {
        this.validateDateFormat(fechaN, e.keyCode);
      };
    }
  }
}
