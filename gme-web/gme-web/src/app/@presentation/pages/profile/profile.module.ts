import { NgModule } from '@angular/core';
import { ProfileComponent } from './profile.component';
import { ThemeModule } from '../../@theme/theme.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { ProfileRoutingModule } from './profile-routing.module';
import { CommonModule } from '@angular/common';
import { PagesComponentsModule } from '../components/components.module';
import {
  NbActionsModule,
  NbCardModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule, NbSelectModule, NbButtonModule,
} from '@nebular/theme';
import { MatCardModule } from '@angular/material/card';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatIconModule } from '@angular/material/icon';

@NgModule({
  declarations: [ProfileComponent],
  imports: [
    ProfileRoutingModule,
    ThemeModule,
    ReactiveFormsModule,
    CommonModule,
    PagesComponentsModule,
    NbCardModule,
    NbActionsModule,
    NbFormFieldModule,
    NbDatepickerModule,
    NbIconModule,
    NbInputModule,
    NbSelectModule,
    FormsModule,
    NbButtonModule,
    MatCardModule,
    CommonComponentsModule,
    MatIconModule

  ]
})

export class ProfileModule {}
