import {
  ComponentFactory,
  ComponentFactoryResolver,
  ComponentRef,
  Directive,
  ElementRef,
  HostBinding,
  Input,
  OnInit,
  Renderer2,
  ViewContainerRef,
} from '@angular/core';
import {AppLoaderComponent} from "./app-loader.component";
import {NbComponentSize, NbComponentStatus} from "@nebular/theme";

// tslint:disable-next-line: directive-selector
@Directive({selector: '[appLoader]'})
export class AppLoaderDirective implements OnInit {

  private shouldShow = false;
  spinner: ComponentRef<AppLoaderComponent>;
  componentFactory: ComponentFactory<AppLoaderComponent>;

  /**
   * Spinner message shown next to the icon
   * @type {string}
   */
  @Input('appLoaderMessage') spinnerMessage: string;

  /**
   * Spinner status color
   * `basic`, `primary`, `info`, `success`, `warning`, `danger`, `control`.
   */
  @Input('appLoaderStatus') spinnerStatus: NbComponentStatus = 'primary';

  /**
   * Spinner size. Possible values: `tiny`, `small`, `medium` (default), `large`, `giant`
   */
  @Input('appLoaderSize') spinnerSize: NbComponentSize = 'medium';

  /**
   * Directive value - show or hide spinner
   * @param {boolean} val
   */
  @Input('appLoader')
  set appLoader(val: boolean) {
    if (this.componentFactory) {
      if (val) {
        this.show();
      } else {
        this.hide();
      }
    } else {
      this.shouldShow = val;
    }
  }


  @HostBinding('class.app-loader-container') isSpinnerExist = false;

  constructor(private directiveView: ViewContainerRef,
              private componentFactoryResolver: ComponentFactoryResolver,
              private renderer: Renderer2,
              private directiveElement: ElementRef) {
  }

  ngOnInit() {
    this.componentFactory = this.componentFactoryResolver.resolveComponentFactory(AppLoaderComponent);
    if (this.shouldShow) {
      this.show();
    }
  }


  hide() {
    if (this.isSpinnerExist) {
      this.directiveView.remove();
      this.isSpinnerExist = false;
    }
  }

  show() {
    if (!this.isSpinnerExist) {
      this.spinner = this.directiveView.createComponent<AppLoaderComponent>(this.componentFactory);
      this.setInstanceInputs(this.spinner.instance);
      this.spinner.changeDetectorRef.detectChanges();
      this.renderer.appendChild(this.directiveElement.nativeElement, this.spinner.location.nativeElement);
      this.isSpinnerExist = true;
    }
  }

  setInstanceInputs(instance: AppLoaderComponent) {
    instance.message = this.spinnerMessage;
    typeof this.spinnerStatus !== 'undefined' && (instance.status = this.spinnerStatus);
    typeof this.spinnerSize !== 'undefined' && (instance.size = this.spinnerSize);
  }

}
