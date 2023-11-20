import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';
import { CreacionBaseService } from '../creacion-base/creacion-base.service';
import gsap from 'gsap';
import { Router } from '@angular/router';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import { Const } from 'src/app/@data/services/const';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';

@Component({
  selector: 'serv-talento-select-base',
  templateUrl: './select-base.component.html',
  styleUrls: ['./select-base.component.scss'],
})
export class SelectBaseComponent implements OnInit {
  @ViewChild('imgCreation', { static: true }) img: ElementRef;
  @ViewChild('formCreation', { static: true }) form: ElementRef;
  @ViewChild('lblForm', { static: true }) label: ElementRef;

  regimenes: any[] = [];
  modalidades: any[] = [];
  tipos: any[] = [];

  registerForm: FormGroup;

  tl = gsap.timeline();
  tiposInforme = [];

  constructor(
    public helperService: CreacionBaseService,
    private entidadService: MaestraEntidadRepository,
    private maestraService: MaestraRepository,
    public router: Router
  ) {}

  ngOnInit(): void {
    this.registerForm = new FormGroup({
      regimen: new FormControl('', [Validators.required]),
      modalidad: new FormControl('', []),
      tipo: new FormControl('', []),
    });

    this.loadRegimenModalidad(null, null, 'regimenes');
  }

  get f() {
    return this.registerForm.controls;
  }

  loadRegimenModalidad(
    regimen: number = null,
    modalidad: number = null,
    list: string
  ) {
    this.entidadService
      .getRegimenModalidad(regimen, modalidad)
      .toPromise()
      .then((res: any[]) => {
        if (list === 'regimenes') {
          this.regimenes = res;
          this.initalizeAnimations();
        } else if (list === 'modalidades') {
          this.modalidades = res.filter((item: any) => {
            return item.codProg !== Const.M_NO_APLICA;
          });

          if (this.modalidades.length > 0) {
            this.registerForm.controls['modalidad'].setValidators([
              Validators.required,
            ]);
          } else {
            this.registerForm.controls['modalidad'].setValidators([]);
          }

          this.registerForm.controls['modalidad'].updateValueAndValidity();
        } else if (list === 'tipos') {
          this.tipos = res;

          if (this.tipos.length > 0) {
            this.registerForm.controls['tipo'].setValidators([
              Validators.required,
            ]);
          } else {
            this.registerForm.controls['tipo'].setValidators([]);
          }

          this.registerForm.controls['tipo'].updateValueAndValidity();
        }
      });
  }

  changeRegimen() {
    this.modalidades = [];
    this.registerForm.controls['modalidad'].setValue('');

    this.tipos = [];
    this.registerForm.controls['tipo'].setValue('');

    this.registerForm.markAsUntouched();

    const regimenValue = this.registerForm.getRawValue().regimen;
    this.loadRegimenModalidad(
      regimenValue.maestraDetalleId,
      null,
      'modalidades'
    );
  }

  changeModalidad() {
    this.tipos = [];
    this.registerForm.controls['tipo'].setValue('');

    this.registerForm.markAsUntouched();

    const regimenValue = this.registerForm.getRawValue().regimen;
    const modalidadValue = this.registerForm.getRawValue().modalidad;
    this.loadRegimenModalidad(
      regimenValue.maestraDetalleId,
      modalidadValue.maestraDetalleId,
      'tipos'
    );
  }

  initalizeAnimations() {
    this.tl.to(this.img.nativeElement, { duration: 1, opacity: 1, x: 0 }, 0);
    this.tl.to(
      this.form.nativeElement,
      { duration: 1.25, opacity: 1, y: 0 },
      0
    );
    this.tl.to(
      this.label.nativeElement,
      { duration: 0.75, opacity: 1, y: 0 },
      0
    );
  }

  createPlantilla() {
    this.helperService.initializeValues();
    this.registerForm.markAllAsTouched();

    if (this.registerForm.invalid) return;
    this.f.regimen.value.codProg === Const.MD_DL1041
      ? (this.helperService.jerarquiaMode = 1)
      : (this.helperService.jerarquiaMode = 0);

    if (this.helperService.jerarquiaMode === 1) {
      this.maestraService
        .getMaestraDetalleByCodandCodProg('TBL_MODALIDAD', +Const.M_NO_APLICA)
        .subscribe((res) => {
          this.f.modalidad.setValue(res[0]);
          this.tipoNoAplica();
        });
    } else {
      this.helperService.jerarquiaSelected = this.registerForm.getRawValue();
      sessionStorage.setItem(
        'jerarquiaSelected',
        JSON.stringify(this.registerForm.getRawValue())
      );

      this.router.navigateByUrl('pages/gestionbases/creacion');
    }
  }

  tipoNoAplica() {
    this.maestraService
      .getMaestraDetalleByCodandCodProg('TBL_TIPO', +Const.T_NO_APLICA)
      .subscribe((res) => {
        this.f.tipo.setValue(res[0]);
        this.helperService.jerarquiaSelected = this.registerForm.getRawValue();
        sessionStorage.setItem(
          'jerarquiaSelected',
          JSON.stringify(this.registerForm.getRawValue())
        );

        this.router.navigateByUrl('pages/gestionbases/creacion');
      });
  }
}
