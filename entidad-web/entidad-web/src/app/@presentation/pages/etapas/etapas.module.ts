import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EtapasComponent } from './etapas.component';
import { EtapasRoutingModule } from './etapas-routing.module';
import { MatDividerModule } from '@angular/material/divider';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbPopoverModule, NbStepperModule, NbCardModule, NbAccordionModule, NbToggleModule, NbRadioModule
} from '@nebular/theme';
import { TableSeguimientoComponent } from './table-seguimiento/table-seguimiento.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatButtonModule } from '@angular/material/button';
import { TooltipInfoReclutamientoComponent } from './tooltip-info/tooltip-info-reclutamiento.component';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { ModalAprobarComponent } from './modal-aprobar/modal-aprobar.component';
import { ResultadosPostulanteComponent } from './resultados-postulante/resultados-postulante.component';
import { ModalCalificarSeccionComponent } from './modal-calificar-seccion/modal-calificar-seccion.component';
import { DialogConfirmRnsscComponent } from './dialog-confirm-rnssc/dialog-confirm-rnssc.component';
import { NgxMatDatetimePickerModule } from '@angular-material-components/datetime-picker';

@NgModule({
  declarations: [
    EtapasComponent,
    TableSeguimientoComponent,
    TooltipInfoReclutamientoComponent,
    ModalAprobarComponent,
    ResultadosPostulanteComponent,
    ModalCalificarSeccionComponent,
    DialogConfirmRnsscComponent,
  ],
  imports: [
    FormsModule,
    MatSlideToggleModule,
    MatProgressBarModule,
    NbPopoverModule,
    CommonModule,
    EtapasRoutingModule,
    MatDividerModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbSelectModule,
    MatPaginatorModule,
    NbDatepickerModule,
    NbIconModule,
    NbButtonModule,
    NbInputModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    CommonComponentsModule,
    MatButtonModule,
    NbCardModule,
    NbStepperModule,
    NbAccordionModule,
    NbToggleModule,
    NbRadioModule,
    NgxMatDatetimePickerModule
  ]
})
export class EtapasModule {}
