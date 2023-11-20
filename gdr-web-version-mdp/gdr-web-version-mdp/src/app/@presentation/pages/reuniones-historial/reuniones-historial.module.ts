import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDividerModule } from '@angular/material/divider';
import { ReunionesHistorialComponent } from './reuniones-historial.component';
import { MatTabsModule } from '@angular/material/tabs';
import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule, NbCardModule, 
  NbDatepickerModule, NbTimepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule, NbPopoverModule, NbRadioModule,
  NbSelectModule, NbTabsetModule, NbTreeGridModule,
  NbButtonGroupModule,
  
} from '@nebular/theme';
import { ReunionesHistorialRoutingModules } from './reuniones-historial-routing.modules';
import { MatTableModule } from '@angular/material/table';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { FormReunionComponent } from './form-reunion/form-reunion.component';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { MatIconModule } from '@angular/material/icon';
import { HistorialReunionesComponent } from './historial-reuniones/historial-reuniones.component';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ModalNotificacionesComponent } from './modal-notificaciones/modal-notificaciones.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { ModalEliminarReunionComponent } from './modal-eliminar-reunion/modal-eliminar-reunion.component';
import { HistorialReunionesEvaluadoComponent } from './historial-reuniones-evaluado/historial-reuniones-evaluado.component';

@NgModule({
  declarations: [
    ReunionesHistorialComponent,
    FormReunionComponent,
    HistorialReunionesComponent,
    HistorialReunionesEvaluadoComponent,
    ModalNotificacionesComponent,
    ModalEliminarReunionComponent,
  ],
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    MatDividerModule,
    MatTabsModule,
    NbCardModule,
    NbSelectModule,
    ReunionesHistorialRoutingModules,
    NbActionsModule,
    NbAutocompleteModule,
    NbButtonModule,
    NbDatepickerModule,
    NbFormFieldModule,
    NbIconModule,
    NbInputModule,
    NbLayoutModule,
    NbPopoverModule,
    NbRadioModule,
    NbSelectModule,
    NbTabsetModule,
    NbTreeGridModule,
    NbTimepickerModule,
    NbButtonGroupModule,
    MatTableModule,
    MatCheckboxModule,
    FontAwesomeModule,
    MatIconModule,
    CommonComponentsModule,
    MatPaginatorModule
  ]
})
export class ReunionesHistorialModule { }
