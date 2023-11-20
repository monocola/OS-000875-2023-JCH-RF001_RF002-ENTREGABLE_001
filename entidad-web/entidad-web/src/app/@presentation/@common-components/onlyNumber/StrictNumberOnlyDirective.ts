import { Directive, ElementRef, HostListener, NgModule } from '@angular/core';

@Directive({
  selector: '[NumberOnly]',
})
export class StrictNumberOnlyDirective {
  private regex: RegExp = new RegExp('/^([0-9]{1,2})(\.)?(\.[0-9]{1,2})?$/');
  constructor(private elementRef: ElementRef) { }

  @HostListener('keydown', ['$event']) onKeyDown(event: KeyboardEvent) {
    let e = event;

    if (
      e.key === 'Backspace' ||
      e.key === 'Tab' ||
      e.key === 'Enter' ||
      e.key === 'Shift' ||
      e.key === 'End' ||
      e.key === 'Home' ||
      e.key === 'ArrowLeft' ||
      e.key === 'ArrowUp' ||
      e.key === 'ArrowRight' ||
      e.key === 'Delete' ||
      e.key === 'AltGraph' ||
      // [8, 9, 13, 27, 46].indexOf(e.keyCode) !== -1 ||
      // Allow: Ctrl+A
      (e.key === 'a' && (e.ctrlKey || e.metaKey)) ||
      // Allow: Ctrl+C
      (e.key === 'c' && (e.ctrlKey || e.metaKey)) ||
      // Allow: Ctrl+V
      (e.key === 'v' && (e.ctrlKey || e.metaKey)) ||
      // Allow: Ctrl+X
      (e.key === 'x' && (e.ctrlKey || e.metaKey))
      // Allow: home, end, left, right
      // (e.keyCode >= 35 && e.keyCode <= 39)
    ) {
      // let it happen, don't do anything
      return;
    }

    const inputValue: string = this.elementRef.nativeElement.value.concat(
      event.key
    );

    if (inputValue && !String(inputValue).match(this.regex)) {
      event.preventDefault();
    }
    return;
  }

  @HostListener('paste', ['$event']) onPaste(event) {
    const clipboardData = (event.originalEvent || event).clipboardData.getData(
      'text/plain'
    );
    if (clipboardData) {
      const regEx: RegExp = new RegExp('/^([0-9]{1,2})(\.)?(\.[0-9]{1,2})?$/');

      if (!regEx.test(clipboardData)) {
        event.preventDefault();
      }
    }
    return;
  }
}

@NgModule({
  declarations: [ StrictNumberOnlyDirective ],
  exports: [ StrictNumberOnlyDirective ]
})
export class StrictNumberOnlyDirectiveModule {}
