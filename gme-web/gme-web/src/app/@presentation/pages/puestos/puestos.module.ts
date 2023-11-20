import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatDividerModule } from '@angular/material/divider';
import { PuestosComponent } from './puestos.component';
import { PuestosRoutingModule } from './puestos-routing.module';
import {
  NbButtonModule,
  NbSelectModule,
  NbFormFieldModule,
  NbInputModule,
  NbRadioModule,
  NbIconModule
} from '@nebular/theme';
import { MatTabsModule } from '@angular/material/tabs';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ModalRegistrarComponent } from './modal-registrar/modal-registrar.component';
import { ModalEliminarComponent } from './modal-eliminar/modal-eliminar.component';
import { ModalConfirmacionObsComponent } from './modal-confirmacion-obs/modal-confirmacion-obs.component';


@NgModule({
  declarations: [
    PuestosComponent,
    ModalRegistrarComponent,
    ModalEliminarComponent,
    ModalConfirmacionObsComponent,
  ],
  imports: [
    PuestosRoutingModule,
    CommonModule,
    MatDividerModule,
    NbButtonModule,
    NbSelectModule,
    MatTabsModule,
    MatCardModule,
    MatIconModule,
    FormsModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbInputModule,
    CommonComponentsModule,
    NbRadioModule,
    NbIconModule,
  ]
})
export class PuestosModule { }
