import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import gsap from 'gsap';
import { BonificacionesHelperService } from '../bonificaciones-helper.service';

@Component({
  selector: 'serv-talento-creacion-bonificacion',
  templateUrl: './creacion-bonificacion.component.html',
  styleUrls: ['./creacion-bonificacion.component.scss'],
})
export class CreacionBonificacionComponent implements OnInit {
  @ViewChild('imgCreation', { static: true }) img: ElementRef;
  @ViewChild('formCreation', { static: true }) form: ElementRef;
  @ViewChild('lblForm', { static: true }) label: ElementRef;

  control = new FormControl('', Validators.required);

  tl = gsap.timeline();

  constructor(
    public helperService: BonificacionesHelperService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.helperService.loadCombox();
    this.initalizeAnimations();
  }

  createPlantilla() {
    this.helperService.bonificacionSelected = this.control.value;
    // this.helperService.initializeValues();
    sessionStorage.setItem(
      'tipoBonificacion',
      JSON.stringify(this.control.value)
    );
    this.router.navigateByUrl('pages/gestionbonificaciones/gestion');
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
