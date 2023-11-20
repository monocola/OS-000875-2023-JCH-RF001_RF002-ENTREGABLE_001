import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { MaestraOrhRoutingModule } from './maestra-orh-routing.module';

import { MaestraOrhComponent } from './maestra-orh.component';
import { MatDividerModule } from '@angular/material/divider';
import { MatTabsModule } from '@angular/material/tabs';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbSelectModule,
  NbToggleModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { ServirSectionComponent } from './servir-section/servir-section.component';
import { EntidadSectionComponent } from './entidad-section/entidad-section.component';
import { MatButtonModule } from '@angular/material/button';
import { ReactiveFormsModule } from '@angular/forms';
import { ModalCreationComponent } from './modal-creation/modal-creation.component';
import { CommonComponentsModule } from '../../@common-components/common-components.module';

@NgModule({
  declarations: [
    MaestraOrhComponent,
    ServirSectionComponent,
    EntidadSectionComponent,
    ModalCreationComponent,
  ],
  imports: [
    CommonModule,
    MaestraOrhRoutingModule,
    MatDividerModule,
    MatTabsModule,
    NbIconModule,
    NbPopoverModule,
    CommonComponentsModule,
    NbButtonModule,
    NbFormFieldModule,
    NbSelectModule,
    NbInputModule,
    NbTreeGridModule,
    MatButtonModule,
    NbToggleModule,
    ReactiveFormsModule,
  ],
})
export class MaestraOrhModule {}
