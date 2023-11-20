import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Const } from 'src/app/@data/services/const';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { CreacionFormBaseService } from '../creacion-form-base/creacion-form-base.service';
import gsap from 'gsap';

@Component({
  selector: 'serv-talento-creacion-plantillas',
  templateUrl: './creacion-plantillas.component.html',
  styleUrls: ['./creacion-plantillas.component.scss'],
})
export class CreacionPlantillasComponent implements OnInit {
  @ViewChild('imgCreation', { static: true }) img: ElementRef;
  @ViewChild('formCreation', { static: true }) form: ElementRef;
  @ViewChild('lblForm', { static: true }) label: ElementRef;

  control = new FormControl('', Validators.required);

  tl = gsap.timeline();
  tiposInforme = [];
  rol: any = JSON.parse(sessionStorage.getItem('roles'));

  constructor(
    private maestraService: MaestraRepository,
    private router: Router,
    private helperService: CreacionFormBaseService
  ) {}

  ngOnInit(): void {
    this.getInformes();
  }

  getInformes() {
    this.maestraService.getMaestraDetalleByCod('TIP_INF').subscribe((res) => {
      this.tiposInforme = Const.R_ADMIN_ENTIDAD !== this.rol.rolId ? res : res.filter(it => it.codProg !== "2");
      this.initalizeAnimations();
    });
  }

  createPlantilla() {
    this.helperService.initializeValues();
    const codProg = this.control.value.codProg;
    this.helperService.tipoInforme = this.control.value;
    sessionStorage.setItem('tipoInforme', JSON.stringify(this.control.value));
    codProg === Const.INF_BONIFICACION
      ? this.router.navigateByUrl('pages/gestionplantillas/bonificaciones')
      : this.router.navigateByUrl('pages/gestionplantillas/base-legal');
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
}
