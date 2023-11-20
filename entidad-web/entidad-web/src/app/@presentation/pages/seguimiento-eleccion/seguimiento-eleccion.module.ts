import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SeguimientoEleccionComponent } from './seguimiento-eleccion.component';
import { SeguimientoEleccionRoutingModule } from './seguimiento-eleccion-routing.module';
import { MatButtonModule } from '@angular/material/button';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbSelectModule,
  NbToggleModule
} from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { TableEleccionComponent } from './table-eleccion/table-eleccion.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ModalContratosComponent } from './modal-contratos/modal-contratos.component';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { ModalRedamComponent } from './modal-redam/modal-redam.component';
import { TooltipEleccionComponent } from './tooltip-eleccion/tooltip-eleccion.component';
import { NotasComponent } from './notas/notas.component';


@NgModule({
  declarations: [
    SeguimientoEleccionComponent,
    TableEleccionComponent,
    ModalContratosComponent,
    ModalRedamComponent,
    TooltipEleccionComponent,
    NotasComponent,
  ],
  imports: [
    CommonModule,
    SeguimientoEleccionRoutingModule,
    MatButtonModule,
    MatProgressBarModule,
    NbIconModule,
    NbFormFieldModule,
    NbButtonModule,
    MatDividerModule,
    ReactiveFormsModule,
    NbSelectModule,
    NbInputModule,
    NbDatepickerModule,
    NbPopoverModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    CommonComponentsModule,
    MatSlideToggleModule,
    FormsModule,
    NbToggleModule
  ]
})
export class SeguimientoEleccionModule { }
