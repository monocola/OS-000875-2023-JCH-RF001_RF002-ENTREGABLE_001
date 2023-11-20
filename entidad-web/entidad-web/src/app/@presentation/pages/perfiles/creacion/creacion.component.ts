import {
  AfterViewInit,
  Component,
  ElementRef,
  OnInit,
  ViewChild,
} from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { CreacionModalComponent } from '../creacion-modal/creacion-modal.component';
import { HelperPerfilesService } from '../helperPerfiles.service';
import { HelperLey276Service } from '../ley276/helperLey276.service';
import { HelperLey30057Service } from '../ley30057/helperLey30057.service';
import { HelperLey1401Service } from '../ley1401/helperLey1401.service';
import gsap from 'gsap';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-creacion',
  templateUrl: './creacion.component.html',
  styleUrls: ['./creacion.component.scss'],
})
export class CreacionComponent implements OnInit, AfterViewInit {
  @ViewChild('imgCreation', { static: true }) img: ElementRef;
  @ViewChild('formCreation', { static: true }) form: ElementRef;
  @ViewChild('lblForm', { static: true }) label: ElementRef;

  modalityField = new FormControl('', Validators.required);
  tl = gsap.timeline();

  constructor(
    private dialog: MatDialog,
    private router: Router,
    private helperLey276: HelperLey276Service,
    private helperLey1401: HelperLey1401Service,
    private helperLey30057: HelperLey30057Service,
    public helperPerfilesService: HelperPerfilesService
  ) {}

  ngAfterViewInit(): void {
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

  ngOnInit(): void {
    this.helperPerfilesService.getRegimenesToCreate();
  }

  openLegendModal() {
    this.dialog.open(CreacionModalComponent, {
      data: this.helperPerfilesService.regimenesToCreate,
      width: '37.5rem',
      maxHeight: '80vh',
    });
  }

  create() {
    const value = this.modalityField.value;
    sessionStorage.setItem('regimenSelected', JSON.stringify(value));
    switch (value.codProg) {
      case Const.MD_DL30057:
        this.helperLey30057.regimenSelected = value;
        this.router.navigateByUrl('/pages/regperfilpuesto/creacion/ley30057');
        break;
      case Const.MD_DL1041:
        this.helperLey1401.regimenSelected = value;
        this.router.navigateByUrl('/pages/regperfilpuesto/creacion/ley1401');
        break;
      default:
        this.helperLey276.regimenSelected = value;
        this.router.navigateByUrl('/pages/regperfilpuesto/creacion/otras');
        break;
    }
  }
}
